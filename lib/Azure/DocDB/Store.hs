{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TemplateHaskell   #-}

module Azure.DocDB.Store (
  CollectionId(..),
  DocumentId(..),
  DBSQL(..),
  DocumentsList(..),
  dbQueryParamSimple,
  createDocument,
  deleteDocument,
  getDocument,
  replaceDocument,
  queryDocuments,
  ) where

import           Control.Lens (makeLenses, set)
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Trans
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Data.Aeson (ToJSON(..), FromJSON(..), Value(..), (.:), (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as L
import qualified Data.List as Lst
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Network.HTTP.Client as HC
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Network.HTTP.Types as HT
import qualified Network.HTTP.Types.Header as HT
import           Network.HTTP.Types.Status (statusIsSuccessful)
import           Web.HttpApiData (ToHttpApiData(..))

import Azure.DocDB.Auth
import Azure.DocDB.ETag
import Azure.DocDB.ResourceId
import Azure.DocDB.SocketMonad
import qualified Azure.DocDB.ServiceHeader as AH


-- | SQL query into the document store
data DBSQL = DBSQL {
  dbSQL :: T.Text,
  dbSQLParams :: [A.Pair]
  } deriving (Eq)


-- | List of documents retrieved from the store
newtype DocumentsList a = DocumentsList {
  documentsListed :: [a]
  } deriving (Eq, Ord)

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

instance FromJSON a => FromJSON (DocumentsList a) where
  parseJSON (Object v) = DocumentsList
    <$> v .: "Documents"


data DBQueryParam = DBQueryParam {
  _maxItemCount :: Int,
  _continuationToken :: T.Text,
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
  -> m (Maybe (ETagged a))
getDocument (ETagged tag res@(DocumentId (CollectionId db coll) docId)) = do
  srsp <- sendSocketRequest SocketRequest {
    srMethod = HT.GET,
    srContent = mempty,
    srResourceType = "docs",
    srResourceLink = resourceLink res,
    srUriPath = "dbs" </> db </> "colls" </> coll </> "docs" </> docId,
    srHeaders = maybeAddHeader ifNoneMatch tag [AH.acceptJSON]
    }

  bdy <- traverse decodeOrThrow $ bodyIfPresent srsp
  return $ ETagged (etagOf srsp) <$> bdy


-- | Retrieve a document from DocumentDB
createDocument :: (DBSocketMonad m, ToJSON a, FromJSON a)
  => CollectionId
  -> a
  -> m (ETagged a)
createDocument res@(CollectionId db coll) doc = do
  (SocketResponse c rhdrs bdy) <- sendSocketRequest SocketRequest {
    srMethod = HT.POST,
    srContent = A.encode doc,
    srResourceType = "docs",
    srResourceLink = resourceLink res,
    srUriPath = "dbs" </> db </> "colls" </> coll </> "docs",
    srHeaders = [AH.acceptJSON, AH.contentJSON]
    }

  ETagged (etagOf rhdrs) <$> decodeOrThrow bdy


-- | Retrieve a document from DocumentDB
replaceDocument :: (DBSocketMonad m, ToJSON a, FromJSON a)
  => ETagged DocumentId
  -> a
  -> m (ETagged a)
replaceDocument (ETagged tag res@(DocumentId (CollectionId db coll) docId)) doc = do
  (SocketResponse c rhdrs bdy) <- sendSocketRequest SocketRequest {
    srMethod = HT.PUT,
    srContent = A.encode doc,
    srResourceType = "docs",
    srResourceLink = resourceLink res,
    srUriPath = "dbs" </> db </> "colls" </> coll </> "docs" </> docId,
    srHeaders = maybeAddHeader ifMatch tag [AH.acceptJSON, AH.contentJSON]
    }

  ETagged (etagOf rhdrs) <$> decodeOrThrow bdy


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
dbQueryParamSimple = DBQueryParam (-1) mempty False


dbQueryParamToHeaders :: DBQueryParam -> [HT.Header]
dbQueryParamToHeaders p =
  prependIf (_maxItemCount p > 0) (AH.maxItemCount, numToB (_maxItemCount p)) $
  prependIf (not . T.null . _continuationToken $ p) (AH.continuation, T.encodeUtf8 $ _continuationToken p) $
  prependIf (_enableCrossPartition p) (AH.queryCrossPartition, "True")
  []
  where
    prependIf :: Bool -> a -> [a] -> [a]
    prependIf False = const id
    prependIf True = (:)

    numToB :: (Show a, Num a) => a -> B.ByteString
    numToB = L.toStrict . B.toLazyByteString . B.stringUtf8 . show


-- | Query the document store for matching records
queryDocuments :: (DBSocketMonad m, FromJSON a)
  => CollectionId
  -> DBSQL
  -> DBQueryParam
  -> m (DBQueryParam, [a])
queryDocuments res@(CollectionId db coll) sql dbQParams = do
  (SocketResponse c rhdrs bdy) <- sendSocketRequest SocketRequest {
    srMethod = HT.POST,
    srContent = A.encode sql,
    srResourceType = "docs",
    srResourceLink = resourceLink res,
    srUriPath = "dbs" </> db </> "colls" </> coll </> "docs",
    srHeaders = dbQueryParamToHeaders dbQParams ++ [AH.isQuery, AH.acceptJSON, AH.contentJSONQuery]
    }
  dcs <- decodeOrThrow bdy
  return (set continuationToken (nextContinuation rhdrs) dbQParams
         , documentsListed dcs)
  where
    -- Get the continuation token for the next request (or empty on the last page)
    nextContinuation :: [HT.Header] -> T.Text
    nextContinuation = maybe T.empty T.decodeUtf8 . lookup AH.continuation
