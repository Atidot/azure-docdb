{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Azure.DocDB.ETag (
  ETag(..),
  ETagged(..),
  ifMatch,
  ifNoneMatch,
  ) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Network.HTTP.Types.Header (Header, hIfMatch, hIfNoneMatch)
import           Web.HttpApiData (ToHttpApiData(..))


-- | An Entity Tag
newtype ETag = ETag T.Text deriving (Eq, Ord, Show)

instance ToHttpApiData ETag where
  toUrlPiece (ETag t) = t
  toHeader (ETag t) = T.encodeUtf8 t

-- | A value which may have an associated entity tag
data ETagged a = ETagged (Maybe ETag) a deriving (Eq, Ord, Show)

etagMerge :: Eq t => Maybe t -> Maybe t -> Maybe t
etagMerge Nothing x = x
etagMerge x Nothing = x
etagMerge x y | x == y = x
etagMerge _ _ = Nothing

instance Functor ETagged where
  fmap f (ETagged t a) = ETagged t (f a)

instance Applicative ETagged where
  pure = ETagged Nothing
  (ETagged t1 f) <*> (ETagged t2 v) = ETagged (etagMerge t1 t2) (f v)

instance Monad ETagged where
  (ETagged t1 a) >>= f = ETagged (etagMerge t1 t2) n
    where
      (ETagged t2 n) = f a


-- TODO: These functions could be made into lenses, allowing for both retrieval
-- and setting an ETag header.
-- :: (Eq a, ToHttpApiData b, FromHttpApiData b) => Lens' [(a, b)] (Maybe b)

ifMatch :: ETag -> Header
ifMatch tag = (hIfMatch, toHeader tag)

ifNoneMatch :: ETag -> Header
ifNoneMatch tag = (hIfNoneMatch, toHeader tag)
