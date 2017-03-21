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

  module Azure.DocDB.Store.List,
  ) where

import           Control.Monad
import           Control.Monad.Except
import           Data.Aeson (ToJSON(..), FromJSON(..))
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import qualified Network.HTTP.Types as HT

--import Azure.DocDB.Auth
import Azure.DocDB.Store.DBDocument
import Azure.DocDB.Store.List
import Azure.DocDB.ETag
import Azure.DocDB.ResourceId
import Azure.DocDB.SocketMonad
import qualified Azure.DocDB.ServiceHeader as AH




-- | Prepend a header to a request, if a value is present to add
maybeAddHeader :: (a -> HT.Header) -> Maybe a -> [HT.Header] -> [HT.Header]
maybeAddHeader mkHeader = maybe id ((:) . mkHeader)


-- | Prepend an item to a list
--maybePrepend :: Maybe b -> [b] -> [b]
--maybePrepend = maybe id (:)

--instance ProvideETag SocketResponse where
--  etagOf = etagOf . srspHeaders

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
getDocument (ETagged tag res@(DocumentId (CollectionId db coll) pdocId)) = do
  srsp <- sendSocketRequest SocketRequest {
    srMethod = HT.GET,
    srContent = mempty,
    srResourceType = "docs",
    srResourceLink = resourceLink res,
    srUriPath = "dbs" </> db </> "colls" </> coll </> "docs" </> pdocId,
    srHeaders = maybeAddHeader ifNoneMatch tag [AH.acceptJSON]
    }

  traverse decodeOrThrow $ bodyIfPresent srsp


-- | Retrieve a document from DocumentDB
createDocument :: (DBSocketMonad m, ToJSON a, FromJSON a)
  => CollectionId
  -> a
  -> m (DBDocument a)
createDocument res@(CollectionId db coll) doc = do
  (SocketResponse _ _ bdy) <- sendSocketRequest SocketRequest {
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
replaceDocument (ETagged tag res@(DocumentId (CollectionId db coll) pdocId)) doc = do
  (SocketResponse _ _ bdy) <- sendSocketRequest SocketRequest {
    srMethod = HT.PUT,
    srContent = A.encode doc,
    srResourceType = "docs",
    srResourceLink = resourceLink res,
    srUriPath = "dbs" </> db </> "colls" </> coll </> "docs" </> pdocId,
    srHeaders = maybeAddHeader ifMatch tag [AH.acceptJSON, AH.contentJSON]
    }

  decodeOrThrow bdy


-- | Delete a document from DocumentDB
deleteDocument :: (DBSocketMonad m)
  => ETagged DocumentId
  -> m ()
deleteDocument (ETagged tag res@(DocumentId (CollectionId db coll) pdocId)) =
  void $ sendSocketRequest SocketRequest {
    srMethod = HT.DELETE,
    srContent = mempty,
    srResourceType = "docs",
    srResourceLink = resourceLink res,
    srUriPath = "dbs" </> db </> "colls" </> coll </> "docs" </> pdocId,
    srHeaders = maybeAddHeader ifMatch tag [AH.acceptJSON, AH.contentJSON]
    }
