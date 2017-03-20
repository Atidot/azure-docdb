{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Azure.DocDB.DocumentStore (
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

import Azure.DocDB.DocDBAuth
import Azure.DocDB.ETag
import Azure.DocDB.DBResourceId
import Azure.DocDB.DBSocketMonad


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
  maxItemCount :: Int,
  continuationToken :: T.Text,
  enableCrossPartition :: Bool
  }


headerContentJSON :: HT.Header
headerContentJSON = (HT.hContentType, "application/json")

headerContentJSONQuery :: HT.Header
headerContentJSONQuery = (HT.hContentType, "application/query+json")

headerAcceptJSON :: HT.Header
headerAcceptJSON = (HT.hAccept, "application/json")

headerIsQuery :: HT.Header
headerIsQuery = ("x-ms-documentdb-isquery", "True")

headerMaxItemCount :: HT.HeaderName
headerMaxItemCount = "x-ms-max-item-count"

headerContinuation :: HT.HeaderName
headerContinuation = "x-ms-continuation"

headerQueryCrossPartition :: HT.HeaderName
headerQueryCrossPartition = "x-ms-documentdb-query-enablecrosspartition"


-- | Prepend a header to a request, if a value is present to add
maybeAddHeader :: (a -> HT.Header) -> Maybe a -> [HT.Header] -> [HT.Header]
maybeAddHeader mkHeader = maybe id ((:) . mkHeader)


-- | Prepend an item to a list
maybePrepend :: Maybe b -> [b] -> [b]
maybePrepend = maybe id (:)


responseETag :: HT.ResponseHeaders -> Maybe ETag
responseETag = fmap decodeETag . lookup HT.hETag
  where
    decodeETag = ETag . T.decodeUtf8


-- | Parse JSON within a failable monad
decodeOrThrow :: (MonadError DBError m, FromJSON a) => L.ByteString -> m a
decodeOrThrow bdy =
  case A.eitherDecode bdy of
    Left err -> throwError $ DBBadRequest (T.pack err)
    Right js -> return js


-- | Retrieve a document from DocumentDB
getDocument :: (DBSocketMonad m, FromJSON a)
  => ETagged DocumentId
  -> m (ETagged a)
getDocument (ETagged tag res@(DocumentId (CollectionId db coll) docId)) = do
  (ETagged etag bdy) <- sendSocketRequest SocketRequest {
    srMethod = HT.GET,
    srContent = mempty,
    srResourceType = "docs",
    srResourceLink = resourceLink res,
    srUriPath = "dbs" </> db </> "colls" </> coll </> "docs" </> docId,
    srHeaders = maybeAddHeader ifNoneMatch tag [headerAcceptJSON]
    }

  ETagged etag <$> decodeOrThrow bdy


-- | Retrieve a document from DocumentDB
createDocument :: (DBSocketMonad m, ToJSON a, FromJSON a)
  => CollectionId
  -> a
  -> m (ETagged a)
createDocument res@(CollectionId db coll) doc = do
  (ETagged etag bdy) <- sendSocketRequest SocketRequest {
    srMethod = HT.POST,
    srContent = A.encode doc,
    srResourceType = "docs",
    srResourceLink = resourceLink res,
    srUriPath = "dbs" </> db </> "colls" </> coll </> "docs",
    srHeaders = [headerAcceptJSON, headerContentJSON]
    }

  ETagged etag <$> decodeOrThrow bdy


-- | Retrieve a document from DocumentDB
replaceDocument :: (DBSocketMonad m, ToJSON a, FromJSON a)
  => ETagged DocumentId
  -> a
  -> m (ETagged a)
replaceDocument (ETagged tag res@(DocumentId (CollectionId db coll) docId)) doc = do
  (ETagged etag bdy) <- sendSocketRequest SocketRequest {
    srMethod = HT.PUT,
    srContent = A.encode doc,
    srResourceType = "docs",
    srResourceLink = resourceLink res,
    srUriPath = "dbs" </> db </> "colls" </> coll </> "docs" </> docId,
    srHeaders = maybeAddHeader ifMatch tag [headerAcceptJSON, headerContentJSON]
    }

  ETagged etag <$> decodeOrThrow bdy


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
    srHeaders = maybeAddHeader ifMatch tag [headerAcceptJSON, headerContentJSON]
    }


-- Simple, default query parameters
dbQueryParamSimple = DBQueryParam (-1) mempty False


dbQueryParamToHeaders :: DBQueryParam -> [HT.Header]
dbQueryParamToHeaders p =
  prependIf (maxItemCount p > 0) (headerMaxItemCount, numToB (maxItemCount p)) $
  prependIf (not . T.null . continuationToken $ p) (headerContinuation, T.encodeUtf8 $ continuationToken p) $
  prependIf (enableCrossPartition p) (headerQueryCrossPartition, "True")
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
  (ETagged etag bdy) <- sendSocketRequest SocketRequest {
    srMethod = HT.POST,
    srContent = A.encode sql,
    srResourceType = "docs",
    srResourceLink = resourceLink res,
    srUriPath = "dbs" </> db </> "colls" </> coll </> "docs",
    srHeaders = dbQueryParamToHeaders dbQParams ++ [headerIsQuery, headerAcceptJSON, headerContentJSONQuery]
    }
  dcs <- decodeOrThrow bdy
  return (dbQParams, documentsListed dcs)
