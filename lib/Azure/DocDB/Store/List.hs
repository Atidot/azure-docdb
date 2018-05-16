{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}


-----------------------------------------------------------------------------
-- | List and query operations for documents
-- Exposes continuation token style calls as well as Conduit
-----------------------------------------------------------------------------

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
  queryFree
  ) where

--

import           Control.Monad.Except
import           Data.Aeson (ToJSON(..), FromJSON(..), Value(..), (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as L
import           Data.Conduit (Source)
import qualified Data.Conduit.List as C
import           Data.Maybe (catMaybes, isJust)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.HTTP.Types as HT

import GHC.Generics          (Generic)
import Data.Typeable         (Typeable)
import Data.Data             (Data)
import Control.DeepSeq       (NFData)

import Azure.DocDB.Store.DBDocument
import Azure.DocDB.ResourceId
import Azure.DocDB.SocketMonad
import qualified Azure.DocDB.ServiceHeader as AH


-- | SQL query into the document store
data DBSQL = DBSQL {
  dBSQL_sql    :: T.Text,
  dBSQL_params :: [A.Pair]
  } deriving (Show, Read, Eq, Typeable, Data, NFData, Generic)


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

-- TODO: maybe fix later. not needed yet
instance FromJSON DBSQL

data DBQueryParam = DBQueryParam {
  maxItemCount :: Maybe Int,
  enableCrossPartition :: Bool
  }


-- | Simple, default query parameters
dbQueryParamSimple :: DBQueryParam
dbQueryParamSimple = DBQueryParam Nothing False


-- | Continuation instructions.  May represent the initial page, or any
-- subsequent page
newtype ContinuationClause = ContinuationClause (Maybe T.Text) deriving (Eq)

-- | The initial continuation in a paging operation.
initialContinuation :: ContinuationClause
initialContinuation = ContinuationClause Nothing

-- | When used after a continuation, indicates whether continuing will
-- progress or reset to the initial page.
mayContinue :: ContinuationClause -> Bool
mayContinue (ContinuationClause t) = isJust t

-- | Get the continuation clause for the next request
-- (possibly reverting to initialContinuation)
nextContinuation :: [HT.Header] -> ContinuationClause
nextContinuation h =
  ContinuationClause (T.decodeUtf8 <$> lookup AH.continuation h)

-- | The continuation header to pass.
thisContinuation :: ContinuationClause -> Maybe (HT.HeaderName, B.ByteString)
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
listDocuments res ctoken = do
  (SocketResponse _ rhdrs bdy) <- sendSocketRequest SocketRequest {
    srMethod = HT.GET,
    srContent = mempty,
    srResourceType = resourceType documentIdProxy,
    srResourceLink = resourceLink res,
    srUriPath = resourcePath documentIdProxy res,
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
queryDocuments dbQParams res sql ctoken = do
  (SocketResponse _ rhdrs bdy) <- sendSocketRequest SocketRequest {
    srMethod = HT.POST,
    srContent = A.encode sql,
    srResourceType = resourceType documentIdProxy,
    srResourceLink = resourceLink res,
    srUriPath = resourcePath documentIdProxy res,
    srHeaders = AH.isQuery
      : AH.acceptJSON
      : AH.contentJSONQuery
      : dbQueryParamToHeaders dbQParams ctoken
    }
  dcs <- decodeOrThrow bdy
  return (nextContinuation rhdrs, documentsListed dcs)
  where

    dbQueryParamToHeaders :: DBQueryParam -> ContinuationClause -> [HT.Header]
    dbQueryParamToHeaders p token = catMaybes
      [ (,) AH.maxItemCount . numToB <$> maxItemCount p
      , thisContinuation token
      , (AH.queryCrossPartition, "True") <$ guard (enableCrossPartition p)
      ]

    numToB :: (Show a, Num a) => a -> B.ByteString
    numToB = L.toStrict . B.toLazyByteString . B.stringUtf8 . show

-- | Query DocumentDB freely
queryFree :: (DBSocketMonad m, FromJSON a)
  => DBQueryParam
  -> CollectionId
  -> DBSQL
  -> ContinuationClause
  -> m (ContinuationClause, [a])
queryFree dbQParams res sql ctoken = do
  (SocketResponse _ rhdrs bdy) <- sendSocketRequest SocketRequest {
    srMethod = HT.POST,
    srContent = A.encode sql,
    srResourceType = resourceType documentIdProxy,
    srResourceLink = resourceLink res,
    srUriPath = resourcePath documentIdProxy res,
    srHeaders = AH.isQuery
      : AH.acceptJSON
      : AH.contentJSONQuery
      : dbQueryParamToHeaders dbQParams ctoken
    }
  dcs <- decodeOrThrow bdy
  return (nextContinuation rhdrs, freeItemsListed dcs)
  where

    dbQueryParamToHeaders :: DBQueryParam -> ContinuationClause -> [HT.Header]
    dbQueryParamToHeaders p token = catMaybes
      [ (,) AH.maxItemCount . numToB <$> maxItemCount p
      , thisContinuation token
      , (AH.queryCrossPartition, "True") <$ guard (enableCrossPartition p)
      ]

    numToB :: (Show a, Num a) => a -> B.ByteString
    numToB = L.toStrict . B.toLazyByteString . B.stringUtf8 . show