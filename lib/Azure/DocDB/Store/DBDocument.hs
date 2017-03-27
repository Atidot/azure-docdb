{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module Azure.DocDB.Store.DBDocument (
  DBDocument(..),
  DocumentsList(..),
  ) where

--
import           Data.Aeson (ToJSON(..), FromJSON(..), Value(..), (.:), (.:?))
import qualified Data.Text as T
import           Azure.DocDB.ETag

-- | Database document, with metadata
data DBDocument a = DBDocument {
  -- | This is the unique name used to identify a document, i.e. no two documents can share the same id. The id must not exceed 255 characters.
  dbdId :: T.Text,
  -- | The resource ID (_rid) is a unique identifier that is also hierarchical per the resource stack on the resource model. It is used internally for placement and navigation of the document resource.
  dbdRId :: T.Text,
  -- | It specifies the last updated timestamp of the resource. The value is a timestamp.
  dbdTimestamp :: Int,
  -- | It is the unique addressable URI for the resource.
  dbdSelf :: T.Text,
  -- | The resource etag required for optimistic concurrency control.
  dbdETag :: ETag,
  -- | The addressable path for the attachments resource.
  dbdAttachments :: Maybe T.Text,
  -- | Custom portion of the document
  dbdCustom :: a
  } deriving (Eq, Show)


instance FromJSON a => FromJSON (DBDocument a) where
  parseJSON x@(Object v) = DBDocument
    <$> v .: "id"
    <*> v .: "_rid"
    <*> v .: "_ts"
    <*> v .: "_self"
    <*> fmap ETag (v .: "_etag")
    <*> v .:? "attachments"
    <*> parseJSON x

instance ProvideETag (DBDocument a) where
  etagOf = Just . dbdETag


-- | List of documents retrieved from the store
newtype DocumentsList a = DocumentsList {
  documentsListed :: [DBDocument a]
  } deriving (Eq)

instance FromJSON a => FromJSON (DocumentsList a) where
  parseJSON (Object v) = DocumentsList
    <$> v .: "Documents"
