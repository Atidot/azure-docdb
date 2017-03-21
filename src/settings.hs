{-# LANGUAGE OverloadedStrings #-}

module Settings (
  AuthSettings(..),
  Settings(..),
  Base64Encoded(..)
  ) where

import           Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

{-
  "accountEndpoint" : ".../",
  "auth": {
    "primary" : "...=",
    "secondary" : "...=",
    "connection" : "...=;"
  },
  "collection" : "...",
  "dB" : "..."
-}

newtype Base64Encoded = Base64Encoded { fromBase64 :: B.ByteString }
  deriving (Eq)

data AuthSettings = AuthSettings {
  primary :: Base64Encoded,
  secondary :: Base64Encoded,
  connection :: T.Text
  }

data Settings = Settings {
  accountEndpoint :: T.Text,
  auth :: AuthSettings,
  collection :: T.Text,
  db :: T.Text
  }

instance FromJSON AuthSettings where
  parseJSON = withObject "AuthSettings" $ \v -> AuthSettings
    <$> v .: "primary"
    <*> v .: "secondary"
    <*> v .: "connection"

instance FromJSON Settings where
  parseJSON = withObject "Settings" $ \v -> Settings
    <$> v .: "accountEndpoint"
    <*> v .: "auth"
    <*> v .: "collection"
    <*> v .: "db"


instance FromJSON Base64Encoded where
  parseJSON (String v) = either fail (pure . Base64Encoded) e
    where
      e = B64.decode (T.encodeUtf8 v)
  parseJSON _ = fail "Base64Encoded"
