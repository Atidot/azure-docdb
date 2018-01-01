{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module Azure.DocDB.Store (
  CollectionId(..),
  DocumentId(..),
  DBDocument(..),
  createDocument,
  deleteDocument,
  getDocument,
  replaceDocument,
  upsertDocument,
  executeStoredProcedure,

  module Azure.DocDB.Store.List,
  ) where

import           Control.Monad
import           Control.Monad.Except
import           Data.Aeson (ToJSON(..), FromJSON(..))
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import qualified Network.HTTP.Types as HT

import           Control.Lens (set)
import           Network.HTTP.Types.Header (hIfMatch, hIfNoneMatch)
import           Web.HttpApiData (ToHttpApiData(..))

import Azure.DocDB.Store.DBDocument
import Azure.DocDB.Store.List
import Azure.DocDB.ETag
import Azure.DocDB.ResourceId
import Azure.DocDB.SocketMonad
import qualified Azure.DocDB.ServiceHeader as AH


-- | Parse JSON within a failable monad
decodeOrThrow :: (MonadError DBError m, FromJSON a) => L.ByteString -> m a
decodeOrThrow bdy =
  case A.eitherDecode bdy of
    Left err -> throwError $ DBBadRequest (T.pack err)
    Right js -> return js


bodyIfPresent :: SocketResponse -> Maybe L.ByteString
bodyIfPresent (SocketResponse 304 _ _) = Nothing
bodyIfPresent (SocketResponse _ _ bdy) = Just bdy

-- | Retrieve a document from DocumentDB
getDocument :: (DBSocketMonad m, FromJSON a)
  => ETagged DocumentId
  -> m (Maybe (DBDocument a))
getDocument (ETagged tag res) = do
  srsp <- sendSocketRequest SocketRequest {
    srMethod = HT.GET,
    srContent = mempty,
    srResourceType = resourceType documentIdProxy,
    srResourceLink = resourceLink res,
    srUriPath = resourceLink res,
    srHeaders =
      set (AH.header' hIfNoneMatch) (toHeader <$> tag)
      [AH.acceptJSON]
    }

  traverse decodeOrThrow $ bodyIfPresent srsp

-- | Retrieve a document from DocumentDB
createDocument :: (DBSocketMonad m, ToJSON a, FromJSON a)
  => CollectionId
  -> a
  -> m (DBDocument a)
createDocument res doc = do
  (SocketResponse _ _ bdy) <- sendSocketRequest SocketRequest {
    srMethod = HT.POST,
    srContent = A.encode doc,
    srResourceType = resourceType documentIdProxy,
    srResourceLink = resourceLink res,
    srUriPath = resourcePath documentIdProxy res,
    srHeaders = [AH.acceptJSON, AH.contentJSON]
    }

  decodeOrThrow bdy


-- | Retrieve a document from DocumentDB
replaceDocument :: (DBSocketMonad m, ToJSON a, FromJSON a)
  => ETagged DocumentId
  -> a
  -> m (DBDocument a)
replaceDocument (ETagged tag res) doc = do
  (SocketResponse _ _ bdy) <- sendSocketRequest SocketRequest {
    srMethod = HT.PUT,
    srContent = A.encode doc,
    srResourceType = resourceType documentIdProxy,
    srResourceLink = resourceLink res,
    srUriPath = resourceLink res,
    srHeaders =
      set (AH.header' hIfMatch) (toHeader <$> tag)
      [AH.acceptJSON, AH.contentJSON]
    }

  decodeOrThrow bdy


-- | Retrieve a document from DocumentDB
upsertDocument :: (DBSocketMonad m, ToJSON a, FromJSON a)
  => CollectionId
  -> a
  -> m (DBDocument a)
upsertDocument res doc = do
  (SocketResponse _ _ bdy) <- sendSocketRequest SocketRequest {
    srMethod = HT.POST,
    srContent = A.encode doc,
    srResourceType = resourceType documentIdProxy,
    srResourceLink = resourceLink res,
    srUriPath = resourcePath documentIdProxy res,
    srHeaders = [AH.acceptJSON, AH.contentJSON, AH.isUpsert]
    }

  decodeOrThrow bdy


-- | Delete a document from DocumentDB
deleteDocument :: (DBSocketMonad m)
  => ETagged DocumentId
  -> m ()
deleteDocument (ETagged tag res) =
  void $ sendSocketRequest SocketRequest {
    srMethod = HT.DELETE,
    srContent = mempty,
    srResourceType = resourceType documentIdProxy,
    srResourceLink = resourceLink res,
    srUriPath = resourceLink res,
    srHeaders =
      set (AH.header' hIfMatch) (toHeader <$> tag)
      [AH.acceptJSON, AH.contentJSON]
    }


-- | Execute a Stored Procedure
executeStoredProcedure :: (DBSocketMonad m, ToJSON a, FromJSON a)
  => StoredProcedureId
  -> a
  -> m A.Value
executeStoredProcedure res params = do
  (SocketResponse _ _ bdy) <- sendSocketRequest SocketRequest {
    srMethod = HT.POST,
    srContent = A.encode params,
    srResourceType = resourceType storedProcedureIdProxy,
    srResourceLink = resourceLink res,
    srUriPath = resourceLink res,
    srHeaders = [AH.acceptJSON, AH.contentJSON]
    }
  decodeOrThrow bdy