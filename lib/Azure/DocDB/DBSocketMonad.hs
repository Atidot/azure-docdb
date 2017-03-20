{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Azure.DocDB.DBSocketMonad (
  DBSocketState,
  DBSocketMonad(..),
  DBSocketT(..),
  SocketRequest(..),
  DBError(..),
  execDBSocketT,
  mkDBSocketState,
  modifyHeaders,
  addHeader,
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Catch (MonadThrow)
import           Control.Monad.Trans
import           Control.Monad.IO.Class
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import           Data.Time.Clock (UTCTime, getCurrentTime)
import qualified Data.Text as T
import qualified Data.Text.IO as T
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


-- | Socket state for DB connections
data DBSocketState = DBSocketState {
  dbSSRequest :: Request,
  -- ^ Verb, uri, body, and additional headers being sent

  sbSSSigning :: SigningParams -> DocDBSignature,
  -- ^ Method to sign requests

  sendHttps :: Request -> IO (Response L.ByteString)
  -- ^ Issue HTTPS requests
  }


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
  sendSocketRequest :: SocketRequest -> m (ETagged L.ByteString)


newtype DBSocketT m a = DBSocketT {
  runDBSocketT :: ExceptT DBError (StateT DBSocketState m) a
  } deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans DBSocketT where
  lift = DBSocketT . lift . lift


headerVersion :: HT.Header
headerVersion = ("x-ms-version", "2016-07-11")

headerSessionToken :: HT.HeaderName
headerSessionToken = "x-ms-session-token"

headerMSDate :: HT.HeaderName
headerMSDate = "x-ms-date"


-- | Execute the DB operation
execDBSocketT :: MonadIO m => DBSocketT m a -> DBSocketState -> m (Either DBError a)
execDBSocketT (DBSocketT m) = evalStateT (runExceptT m)


--- | DBSocketState constructor
mkDBSocketState :: (MonadThrow m, MonadIO m, Alternative m)
  => B.ByteString   -- ^ Signing key
  -> T.Text         -- ^ Root url
  -> Manager        -- ^ Network manager
  -> m DBSocketState
mkDBSocketState signingKey root mgr = do
  r <- parseRequest $ T.unpack root
  return DBSocketState
    { dbSSRequest = r { requestHeaders = [headerVersion] }
    , sbSSSigning = signRequestInfo signingKey
    , sendHttps = mkDebuggable (`httpLbs` mgr)
    }

-- | Add IO printing to network activity
mkDebuggable :: MonadIO m
  => (Request -> m (Response L.ByteString))
  -> Request
  -> m (Response L.ByteString)
mkDebuggable f req = do
  liftIO $ do
    print req
    T.putStrLn (case requestBody req of
      RequestBodyLBS lb -> T.decodeUtf8 $ L.toStrict lb
      RequestBodyBS sb -> T.decodeUtf8 sb)
  rspTmp <- f req
  liftIO $ print rspTmp
  return rspTmp


responseETag :: HT.ResponseHeaders -> Maybe ETag
responseETag = fmap decodeETag . lookup HT.hETag
  where
    decodeETag = ETag . T.decodeUtf8


instance Monad m => MonadError DBError (DBSocketT m) where
  throwError e = DBSocketT $ throwError e
  catchError (DBSocketT ma) fema = DBSocketT $ catchError ma (runDBSocketT . fema)


instance MonadIO m => DBSocketMonad (DBSocketT m) where
  sendSocketRequest socketRequest = DBSocketT $ do
    (DBSocketState req fsign sendHttps) <- get
    -- Pick a timestamp for signing
    now <- MSDate <$> liftIO getCurrentTime

    let signature = fsign SigningParams {
      spMethod = srMethod socketRequest,
      spResourceType = srResourceType socketRequest,
      spPath = srResourceLink socketRequest,
      spWhen = now
      }

    response <- liftIO $ sendHttps (withUpdatedHeaders now signature req)

    let status = responseStatus response
    let statusText = T.decodeUtf8 . HT.statusMessage $ status
    let hdrs = responseHeaders response

    case HT.statusCode status of
      403 -> throwError DBForbidden
      404 -> throwError DBNotFound
      409 -> throwError DBConflict
      412 -> throwError DBPreconditionFailure
      413 -> throwError DBEntityTooLarge
      code | code >= 400 && code < 500 -> throwError $ DBBadRequest statusText
      code | code >= 500 -> throwError $ DBServiceError statusText
      _ -> return $ ETagged (responseETag hdrs) (responseBody response)
    --
    where
      withUpdatedHeaders :: ToHttpApiData a => MSDate -> a -> Request  -> Request
      withUpdatedHeaders when docDBSignature =
        (\req -> req {
          path = path req </> T.encodeUtf8 (srUriPath socketRequest),
          method = HT.renderStdMethod $ srMethod socketRequest,
          requestBody = RequestBodyLBS (srContent socketRequest)
          }) .
        modifyHeaders (\headers -> headers ++ srHeaders socketRequest) .
        addHeader (headerMSDate, toHeader when) .
        addHeader (HT.hAuthorization, toHeader docDBSignature)


-- | Modify headers on a request
modifyHeaders :: ([HT.Header] -> [HT.Header]) -> Request -> Request
modifyHeaders f req =
  req { requestHeaders = f . requestHeaders $ req }

-- | Prepend a header to a request
addHeader :: HT.Header -> Request -> Request
addHeader h = modifyHeaders ((:) h)
