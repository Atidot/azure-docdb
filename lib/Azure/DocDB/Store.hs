{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TemplateHaskell   #-}

module Azure.DocDB.Store (
  CollectionId(..),
  DocumentId(..),
  DBDocument(..),
  DBSQL(..),
  DocumentsList(..),
  dbQueryParamSimple,
  createDocument,
  deleteDocument,
  getDocument,
  replaceDocument,
  queryDocuments,
  ) where

import           Control.Lens (makeLenses, set, (^.), (.~), (&))
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Trans
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Data.Aeson (ToJSON(..), FromJSON(..), Value(..), (.:), (.:?), (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as L
import qualified Data.List as Lst
import           Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Network.HTTP.Client as HC
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Network.HTTP.Types as HT
import qualified Network.HTTP.Types.Header as HT
import           Network.HTTP.Types.Status (statusIsSuccessful)
import           Web.HttpApiData (ToHttpApiData(..))

import Azure.DocDB.Auth
import Azure.DocDB.Store.DBDocument
import Azure.DocDB.ETag
import Azure.DocDB.ResourceId
import Azure.DocDB.SocketMonad
import qualified Azure.DocDB.ServiceHeader as AH


-- | SQL query into the document store
data DBSQL = DBSQL {
  dbSQL :: T.Text,
  dbSQLParams :: [A.Pair]
  } deriving (Eq)


instance ToJSON DBSQL where
  toJSON (DBSQL sql prms) = A.object [
    "query" .= sql,
    "parameters" .= (toNameValue <$> prms)
    ]
    where
      toNameValue :: ToJSON a => (T.Text, a) -> Value
      toNameValue (k, v) = A.object [
        "value" .= v,
        "name" .= k
        ]


data DBQueryParam = DBQueryParam {
  _maxItemCount :: Maybe Int,
  _continuationToken :: Maybe T.Text,
  _enableCrossPartition :: Bool
  }

makeLenses ''DBQueryParam


-- | Prepend a header to a request, if a value is present to add
maybeAddHeader :: (a -> HT.Header) -> Maybe a -> [HT.Header] -> [HT.Header]
maybeAddHeader mkHeader = maybe id ((:) . mkHeader)


-- | Prepend an item to a list
maybePrepend :: Maybe b -> [b] -> [b]
maybePrepend = maybe id (:)

instance ProvideETag SocketResponse where
  etagOf = etagOf . srspHeaders

-- | Parse JSON within a failable monad
decodeOrThrow :: (MonadError DBError m, FromJSON a) => L.ByteString -> m a
decodeOrThrow bdy =
  case A.eitherDecode bdy of
    Left err -> throwError $ DBBadRequest (T.pack err)
    Right js -> return js


bodyIfPresent :: SocketResponse -> Maybe L.ByteString
bodyIfPresent (SocketResponse 304 _ bdy) = Nothing
bodyIfPresent (SocketResponse _ _ bdy) = Just bdy

-- | Retrieve a document from DocumentDB
getDocument :: (DBSocketMonad m, FromJSON a)
  => ETagged DocumentId
  -> m (Maybe (DBDocument a))
getDocument (ETagged tag res@(DocumentId (CollectionId db coll) docId)) = do
  srsp <- sendSocketRequest SocketRequest {
    srMethod = HT.GET,
    srContent = mempty,
    srResourceType = "docs",
    srResourceLink = resourceLink res,
    srUriPath = "dbs" </> db </> "colls" </> coll </> "docs" </> docId,
    srHeaders = maybeAddHeader ifNoneMatch tag [AH.acceptJSON]
    }

  traverse decodeOrThrow $ bodyIfPresent srsp


-- | Retrieve a document from DocumentDB
createDocument :: (DBSocketMonad m, ToJSON a, FromJSON a)
  => CollectionId
  -> a
  -> m (DBDocument a)
createDocument res@(CollectionId db coll) doc = do
  (SocketResponse c rhdrs bdy) <- sendSocketRequest SocketRequest {
    srMethod = HT.POST,
    srContent = A.encode doc,
    srResourceType = "docs",
    srResourceLink = resourceLink res,
    srUriPath = "dbs" </> db </> "colls" </> coll </> "docs",
    srHeaders = [AH.acceptJSON, AH.contentJSON]
    }

  decodeOrThrow bdy


-- | Retrieve a document from DocumentDB
replaceDocument :: (DBSocketMonad m, ToJSON a, FromJSON a)
  => ETagged DocumentId
  -> a
  -> m (DBDocument a)
replaceDocument (ETagged tag res@(DocumentId (CollectionId db coll) docId)) doc = do
  (SocketResponse c rhdrs bdy) <- sendSocketRequest SocketRequest {
    srMethod = HT.PUT,
    srContent = A.encode doc,
    srResourceType = "docs",
    srResourceLink = resourceLink res,
    srUriPath = "dbs" </> db </> "colls" </> coll </> "docs" </> docId,
    srHeaders = maybeAddHeader ifMatch tag [AH.acceptJSON, AH.contentJSON]
    }

  decodeOrThrow bdy


-- | Delete a document from DocumentDB
deleteDocument :: (DBSocketMonad m)
  => ETagged DocumentId
  -> m ()
deleteDocument (ETagged tag res@(DocumentId (CollectionId db coll) docId)) =
  void $ sendSocketRequest SocketRequest {
    srMethod = HT.DELETE,
    srContent = mempty,
    srResourceType = "docs",
    srResourceLink = resourceLink res,
    srUriPath = "dbs" </> db </> "colls" </> coll </> "docs" </> docId,
    srHeaders = maybeAddHeader ifMatch tag [AH.acceptJSON, AH.contentJSON]
    }


-- Simple, default query parameters
dbQueryParamSimple = DBQueryParam Nothing Nothing False


dbQueryParamToHeaders :: DBQueryParam -> [HT.Header]
dbQueryParamToHeaders p = catMaybes
  [ (,) AH.maxItemCount . numToB <$> (p ^. maxItemCount)
  , (,) AH.continuation . T.encodeUtf8 <$> (p ^. continuationToken)
  , (AH.queryCrossPartition, "True") <$ guard (p ^. enableCrossPartition)
  ]
  where
    numToB :: (Show a, Num a) => a -> B.ByteString
    numToB = L.toStrict . B.toLazyByteString . B.stringUtf8 . show


-- | Query DocumentDB for documents matching the query provided
queryDocuments :: (MonadState DBQueryParam m, DBSocketMonad m, FromJSON a)
  => CollectionId
  -> DBSQL
  -> m [DBDocument a]
queryDocuments res@(CollectionId db coll) sql = do
  dbQParams <- get
  (SocketResponse c rhdrs bdy) <- sendSocketRequest SocketRequest {
    srMethod = HT.POST,
    srContent = A.encode sql,
    srResourceType = "docs",
    srResourceLink = resourceLink res,
    srUriPath = "dbs" </> db </> "colls" </> coll </> "docs",
    srHeaders = dbQueryParamToHeaders dbQParams ++ [AH.isQuery, AH.acceptJSON, AH.contentJSONQuery]
    }
  dcs <- decodeOrThrow bdy
  put (dbQParams & continuationToken .~ nextContinuation rhdrs)
  return $ documentsListed dcs
  where
    -- Get the continuation token for the next request (or empty on the last page)
    nextContinuation :: [HT.Header] -> Maybe T.Text
    nextContinuation h =
      T.decodeUtf8 <$> lookup AH.continuation h
