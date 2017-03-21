{-# LANGUAGE FlexibleContexts #-}

-----------------------------------------------------------------------------
-- | DBSocketMonad typeclass and related types.
-- Used as a replacement in replacement of Network.HTTP Request/Response
-- in order to carry additional data needed for request signing.
-----------------------------------------------------------------------------

module Azure.DocDB.SocketMonad.Class (
  DBError(..),
  SocketRequest(..),
  SocketResponse(..),
  SessionToken(..),

  DBSocketMonad(..),
  DBSessionMonad(..),
  ) where

--
import           Control.Monad.Except (MonadError)
import           Control.Monad.Reader (ReaderT)
import           Control.Monad.Trans (MonadTrans(..))
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import qualified Network.HTTP.Types as HT

-- | Socket request
data SocketRequest = SocketRequest {
-- | Method for action
  srMethod :: HT.StdMethod,

  -- | Type of resource being accessed
  srResourceType :: T.Text,

  -- | Link for resource, per auth
  srResourceLink :: T.Text,

  -- | Path to the resource to access
  srUriPath :: T.Text,

  -- | Additional headers to pass
  srHeaders :: [HT.Header],

  -- | Content JSON
  srContent :: L.ByteString
  }


data SocketResponse = SocketResponse {
  srspStatusCode :: Int,

  -- | Additional headers to pass
  srspHeaders :: [HT.Header],

  -- | Content JSON
  srspContent :: L.ByteString

  }


-- | A token used with session level consistency.
newtype SessionToken = SessionToken {
  sessionTokenValue :: B.ByteString
  } deriving (Eq)



data DBError
  = DBServiceError T.Text
  | DBEntityTooLarge
  | DBPreconditionFailure
  | DBConflict
  | DBRequestTimeout
  | DBNotFound
  | DBForbidden
  | DBBadRequest T.Text
  deriving (Show)


-- | A socket monad can send requests and receive response bodies
class MonadError DBError m => DBSocketMonad m where
  -- | Send a socket request
  sendSocketRequest :: SocketRequest -> m SocketResponse

-- | Monad that stores session information
class Monad m => DBSessionMonad m where
  overSession :: (Maybe SessionToken -> (Maybe SessionToken, a)) -> m a


--
instance DBSocketMonad m => DBSocketMonad (ReaderT s m) where
  sendSocketRequest = lift . sendSocketRequest
