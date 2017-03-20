{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Azure.DocDB.DBResourceId (
  DBResourceId(..),
  CollectionId(..),
  DocumentId(..),
  ExtendPath(..),
  (</>),
  ) where


import qualified Data.Text as T
import Data.String (IsString)

-- | A DB resource can provide resourceLinks used for signing
class DBResourceId a where
  resourceLink :: a -> T.Text


-- | Identify a collection of documents.
-- "A collection is a container of JSON documents and associated JavaScript
-- application logic."
data CollectionId = CollectionId {
  databaseId :: T.Text,
  collectionId :: T.Text
  } deriving (Eq, Ord)


instance DBResourceId CollectionId where
  resourceLink (CollectionId d c) = "dbs" </> d </> "colls" </> c

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

instance DBResourceId DocumentId where
  resourceLink (DocumentId c d) = resourceLink c </> "docs" </> d

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
