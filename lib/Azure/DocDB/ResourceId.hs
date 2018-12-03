{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Azure.DocDB.ResourceId (
  DBResourceId(..),
  CollectionId(..),
  DocumentId(..),
  StoredProcedureId(..),
  ExtendPath(..),
  (</>),
  collectionIdProxy,
  documentIdProxy,
  storedProcedureIdProxy
  ) where


import qualified Data.Text as T
import           Data.String (IsString)
import           Data.Proxy

-- | A DB resource can provide resourceLinks used for signing
class DBResourceId a b | a -> b where
  -- | The resource link for an instance of a resource
  resourceLink :: a -> T.Text
  -- | The resource path for the resource, under which resources would be held
  resourcePath :: Proxy a -> b -> T.Text
  -- | Type of this resource (for signing)
  resourceType :: Proxy a -> T.Text


-- | Identify a collection of documents.
-- "A collection is a container of JSON documents and associated JavaScript
-- application logic."
data CollectionId = CollectionId {
  databaseId :: T.Text,
  collectionId :: T.Text
  } deriving (Eq, Ord)

collectionIdProxy :: Proxy CollectionId
collectionIdProxy = Proxy



instance DBResourceId CollectionId T.Text where
  resourceLink (CollectionId d c) = resourcePath collectionIdProxy d </> c
  resourcePath p db = "dbs" </> db </> resourceType p
  resourceType _ = "colls"


-- | Identify a document
-- "The document resource is represented by docs in the DocumentDB resource model.
-- A document consists of user-defined content in JSON format. Aside from the
-- required id property, users can define any arbitrary elements, structures,
-- and hierarchies for the content. The id element is a unique string that is
-- user-settable and must not exceed 255 characters."
data DocumentId = DocumentId {
  collection :: CollectionId,
  docId :: T.Text
  } deriving (Eq, Ord)

documentIdProxy :: Proxy DocumentId
documentIdProxy = Proxy

instance DBResourceId DocumentId CollectionId where
  resourceLink (DocumentId c d) = resourcePath documentIdProxy c </> d
  resourcePath p c = resourceLink c </> resourceType p
  resourceType _ = "docs"

-- | Identify a stored procedure
data StoredProcedureId = StoredProcedureId {
  collectionSP :: CollectionId,
  sprocId :: T.Text
  } deriving (Eq, Ord)

storedProcedureIdProxy :: Proxy StoredProcedureId
storedProcedureIdProxy = Proxy

instance DBResourceId StoredProcedureId CollectionId where
  resourceLink (StoredProcedureId c d) = resourcePath storedProcedureIdProxy c </> d
  resourcePath p c = resourceLink c </> resourceType p
  resourceType _ = "sprocs"

-- | Concatenate two string-likes, separating them with a slash ('/')
(</>) :: (IsString a, Monoid a) => a -> a -> a
a </> b = a `mappend` "/" `mappend` b


-- | Extend a "path" object
class ExtendPath a b c | c -> a, c -> b where
  (#>) :: a -> b -> c

instance ExtendPath T.Text T.Text CollectionId where
  (#>) = CollectionId

instance ExtendPath CollectionId T.Text DocumentId where
  (#>) = DocumentId

instance ExtendPath CollectionId T.Text StoredProcedureId where
  (#>) = StoredProcedureId