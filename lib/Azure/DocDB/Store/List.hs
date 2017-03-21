{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module Azure.DocDB.Store.List (
  ContinuationClause(..),
  DBSQL(..),
  DocumentsList(..),

  DBQueryParam(..),
  dbQueryParamSimple,

  initialContinuation,
  listAll,
  listDocuments,
  queryDocuments,
  ) where

--

import           Control.Monad.Except
import           Data.Aeson (ToJSON(..), FromJSON(..), Value(..), (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as L
import           Data.Conduit (Source, yield)
import qualified Data.Conduit as C
import qualified Data.Conduit.List as C
import           Data.Maybe (catMaybes, isJust)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Network.HTTP.Client as HC
import qualified Network.HTTP.Types as HT
import qualified Network.HTTP.Types.Header as HT


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
  maxItemCount :: Maybe Int,
  enableCrossPartition :: Bool
  }


-- | Simple, default query parameters
dbQueryParamSimple = DBQueryParam Nothing False


-- | Continuation instructions.  May represent the initial page, or any
-- subsequent page
newtype ContinuationClause = ContinuationClause (Maybe T.Text) deriving (Eq)

-- | The initial continuation in a paging operation.
initialContinuation = ContinuationClause Nothing

-- | When used after a continuation, indicates whether continuing will
-- progress or reset to the initial page.
mayContinue (ContinuationClause t) = isJust t

-- | Get the continuation clause for the next request
-- (possibly reverting to initialContinuation)
nextContinuation :: [HT.Header] -> ContinuationClause
nextContinuation h =
  ContinuationClause (T.decodeUtf8 <$> lookup AH.continuation h)

-- | The continuation header to pass.
thisContinuation (ContinuationClause Nothing) = Nothing
thisContinuation (ContinuationClause (Just text)) =
  Just (AH.continuation, T.encodeUtf8 text)


-- | Parse JSON within a failable monad
decodeOrThrow :: (MonadError DBError m, FromJSON a) => L.ByteString -> m a
decodeOrThrow bdy =
  case A.eitherDecode bdy of
    Left err -> throwError $ DBBadRequest (T.pack err)
    Right js -> return js


-- | Retrieve all document from a collection
listAll :: (Monad m)
  => (ContinuationClause -> m (ContinuationClause, [a]))
  -> Source m a
listAll fpage = page initialContinuation
  where
    page ctoken = do
      (continuationClause, items) <- lift $ fpage ctoken

      -- This page
      C.sourceList items

      -- Next page
      when (mayContinue continuationClause) (page continuationClause)


-- | Retrieve a set of documents from collection
listDocuments :: (DBSocketMonad m, ToJSON a, FromJSON a)
  => CollectionId
  -> ContinuationClause
  -> m (ContinuationClause, [DBDocument a])
listDocuments res@(CollectionId db coll) ctoken = do
  (SocketResponse c rhdrs bdy) <- sendSocketRequest SocketRequest {
    srMethod = HT.GET,
    srContent = mempty,
    srResourceType = "docs",
    srResourceLink = resourceLink res,
    srUriPath = "dbs" </> db </> "colls" </> coll </> "docs",
    srHeaders = AH.acceptJSON : catMaybes [thisContinuation ctoken]
    }

  docList <- decodeOrThrow bdy
  return (nextContinuation rhdrs, documentsListed docList)


-- | Query DocumentDB for documents matching the query provided
queryDocuments :: (DBSocketMonad m, FromJSON a)
  => DBQueryParam
  -> CollectionId
  -> DBSQL
  -> ContinuationClause
  -> m (ContinuationClause, [DBDocument a])
queryDocuments dbQParams res@(CollectionId db coll) sql ctoken = do
  (SocketResponse c rhdrs bdy) <- sendSocketRequest SocketRequest {
    srMethod = HT.POST,
    srContent = A.encode sql,
    srResourceType = "docs",
    srResourceLink = resourceLink res,
    srUriPath = "dbs" </> db </> "colls" </> coll </> "docs",
    srHeaders = AH.isQuery
      : AH.acceptJSON
      : AH.contentJSONQuery
      : dbQueryParamToHeaders dbQParams ctoken
    }
  dcs <- decodeOrThrow bdy
  return (nextContinuation rhdrs, documentsListed dcs)
  where

    dbQueryParamToHeaders :: DBQueryParam -> ContinuationClause -> [HT.Header]
    dbQueryParamToHeaders p ctoken = catMaybes
      [ (,) AH.maxItemCount . numToB <$> maxItemCount p
      , thisContinuation ctoken
      , (AH.queryCrossPartition, "True") <$ guard (enableCrossPartition p)
      ]

    numToB :: (Show a, Num a) => a -> B.ByteString
    numToB = L.toStrict . B.toLazyByteString . B.stringUtf8 . show
