{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE UndecidableInstances       #-}

-----------------------------------------------------------------------------
-- Enable DocumentDB session level consistency by chaining the
-- x-ms-session-token from response to request.
-- This is left as an optional addition to the base socket monad, for use
-- when this type of consistency is needed.
-----------------------------------------------------------------------------

module Azure.DocDB.SessionMonad (
  SessionToken(..),
  DBSessionMonad(..),
  DBSessionT,
  runDBSessionT,
  evalDBSessionT,
  ) where

import           Control.Lens (Lens', lens, (&), (.~), (^.))
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Trans
import qualified Data.ByteString as B
import           Data.Maybe (fromMaybe)
import qualified Network.HTTP.Types.Header as HT

import           Azure.DocDB.SocketMonad
import qualified Azure.DocDB.ServiceHeader as AH


-- | A token used with session level consistency.
newtype SessionToken = SessionToken {
  sessionTokenValue :: B.ByteString
  } deriving (Eq)


-- | Monad that stores session information
class Monad m => DBSessionMonad m where
  overSession :: (Maybe SessionToken -> (Maybe SessionToken, a)) -> m a


-- | DocumentDB session chaining transformer.  Captures session headers
-- and chains them into subsequent HTTP requests to maintain session consistency.
newtype DBSessionT m a = DBSessionT {
  runSessionT :: StateT (Maybe SessionToken) m a
  } deriving (Functor, Applicative, Monad, MonadIO)


instance MonadTrans DBSessionT where
  lift = DBSessionT . lift


instance (MonadError e m) => MonadError e (DBSessionT m)  where
  throwError = lift . throwError
  catchError (DBSessionT ma) fema = DBSessionT $ catchError ma (runSessionT . fema)


instance Monad m => DBSessionMonad (DBSessionT m) where
  overSession f = DBSessionT $ do
    (nt, r) <- gets f
    put nt
    return r


instance (MonadIO m, DBSocketMonad m) => DBSocketMonad (DBSessionT m) where
  sendSocketRequest socketRequest = DBSessionT $ do
    session <- gets $ fmap sessionTokenValue

    -- Modify the sessionToken header to match the current session
    let socketRequest' = socketRequest
                         & (srHeaders' . AH.header' AH.sessionToken)
                         .~ session

    -- Send
    rsp <- lift $ sendSocketRequest socketRequest'

    -- Update the session
    put $ SessionToken <$> rsp ^. (srspHeaders' . AH.header' AH.sessionToken)

    return rsp


-- | Run a session, from a known initial session
runDBSessionT :: DBSessionT m a -> Maybe SessionToken -> m (a, Maybe SessionToken)
runDBSessionT = runStateT . runSessionT


-- | Evaluate an operation, tracking state between requests.
-- The initial request will be made without a session, subsequent requests
-- will chain the session token.
evalDBSessionT :: Functor m => DBSessionT m a -> m a
evalDBSessionT m = fst <$> runDBSessionT m Nothing


--

srHeaders' :: Lens' SocketRequest [HT.Header]
srHeaders' = lens srHeaders (\req m -> req { srHeaders = m })

srspHeaders' :: Lens' SocketResponse [HT.Header]
srspHeaders' = lens srspHeaders (\rsp m -> rsp { srspHeaders = m })
