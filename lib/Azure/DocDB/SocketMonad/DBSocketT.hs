{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Azure.DocDB.SocketMonad.DBSocketT (
  DBSocketState,
  DBSocketT,
  execDBSocketT,
  mkDBSocketState
  ) where

import           Control.Applicative
import           Control.Lens (Lens', lens, over, set)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Catch (MonadThrow)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import           Data.Time.Clock (getCurrentTime)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import           Network.HTTP.Client as HC
import qualified Network.HTTP.Types as HT
import           Web.HttpApiData (ToHttpApiData(..))

import Azure.DocDB.Auth
--import Azure.DocDB.ETag
import Azure.DocDB.ResourceId
import qualified Azure.DocDB.ServiceHeader as AH
import Azure.DocDB.SocketMonad.Class

-- | Socket state for DB connections
data DBSocketState = DBSocketState {
  dbSSRequest :: Request,
  -- ^ Verb, uri, body, and additional headers being sent

  sbSSSigning :: SigningParams -> DocDBSignature,
  -- ^ Method to sign requests

  sendHttps :: Request -> IO (Response L.ByteString)
  }


newtype DBSocketT m a = DBSocketT {
  runDBSocketT :: ExceptT DBError (ReaderT DBSocketState m) a
  } deriving (Functor, Applicative, Monad, MonadIO)


instance MonadTrans DBSocketT where
  lift = DBSocketT . lift . lift


-- Lenses for a request
method' :: Lens' Request HT.Method
method' = lens method (\req m -> req { method = m })

path' :: Lens' Request B.ByteString
path' = lens path (\req m -> req { path = m })

requestBody' :: Lens' Request RequestBody
requestBody' = lens requestBody (\req m -> req { requestBody = m })

requestHeaders' :: Lens' Request [HT.Header]
requestHeaders' = lens requestHeaders (\req m -> req { requestHeaders = m })


-- | Execute the DB operation
execDBSocketT :: MonadIO m => DBSocketT m a -> DBSocketState -> m (Either DBError a)
execDBSocketT (DBSocketT m) = runReaderT (runExceptT m)


--- | DBSocketState constructor
mkDBSocketState :: (MonadThrow m, MonadIO m, Alternative m)
  => B.ByteString   -- ^ Signing key
  -> T.Text         -- ^ Root url
  -> Manager        -- ^ Network manager
  -> m DBSocketState
mkDBSocketState signingKey root mgr = do
  r <- parseRequest $ T.unpack root
  return DBSocketState
    { dbSSRequest = r { requestHeaders = [AH.version] }
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
      RequestBodyBS sb -> T.decodeUtf8 sb
      _ -> "Unknown response")
  rspTmp <- f req
  liftIO $ print rspTmp
  return rspTmp


instance Monad m => MonadError DBError (DBSocketT m) where
  throwError e = DBSocketT $ throwError e
  catchError (DBSocketT ma) fema = DBSocketT $ catchError ma (runDBSocketT . fema)


instance MonadIO m => DBSocketMonad (DBSocketT m) where
  sendSocketRequest socketRequest = DBSocketT $ do
    (DBSocketState req fsign sendHttpsProc) <- ask
    -- Pick a timestamp for signing
    now <- MSDate <$> liftIO getCurrentTime

    -- Sign the request
    let signature = fsign SigningParams {
      spMethod = srMethod socketRequest,
      spResourceType = srResourceType socketRequest,
      spPath = srResourceLink socketRequest,
      spWhen = now
      }

    -- Build and issue the request
    response <- liftIO
      . sendHttpsProc
      . setRequestContent
      . setRequestPath
      . setRequestMethod
      . withUpdatedHeaders now signature
      $ req

    let status = responseStatus response
    let statusText = T.decodeUtf8 . HT.statusMessage $ status

    case HT.statusCode status of
      403 -> throwError DBForbidden
      404 -> throwError DBNotFound
      409 -> throwError DBConflict
      412 -> throwError DBPreconditionFailure
      413 -> throwError DBEntityTooLarge
      code | code >= 400 && code < 500 -> throwError $ DBBadRequest statusText
      code | code >= 500 -> throwError $ DBServiceError statusText
      _ -> return . responseToSocketResponse $ response
    --
    where
      -- Assign the outgoing request method, path, and content
      setRequestMethod, setRequestPath, setRequestContent :: Request  -> Request
      setRequestMethod =
          set method' (HT.renderStdMethod $ srMethod socketRequest)
      setRequestPath =
          over path' (</> T.encodeUtf8 (srUriPath socketRequest))
      setRequestContent =
          set requestBody' (RequestBodyLBS (srContent socketRequest))
      -- Append new headers for the outgoing request
      withUpdatedHeaders :: ToHttpApiData a => MSDate -> a -> Request -> Request
      withUpdatedHeaders dateWhen docDBSignature = over requestHeaders' $
        set (AH.header' AH.msDate) (Just $ toHeader dateWhen) .
        set (AH.header' HT.hAuthorization) (Just $ toHeader docDBSignature) .
        (srHeaders socketRequest ++)

      responseToSocketResponse :: Response L.ByteString -> SocketResponse
      responseToSocketResponse response = SocketResponse
        (HT.statusCode $ responseStatus response)
        (responseHeaders response)
        (responseBody response)