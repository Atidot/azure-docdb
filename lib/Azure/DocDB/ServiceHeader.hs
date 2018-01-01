{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Azure.DocDB.ServiceHeader where

import Control.Lens (lens)
import qualified Network.HTTP.Types.Header as HT


contentJSON :: HT.Header
contentJSON = (HT.hContentType, "application/json")

contentJSONQuery :: HT.Header
contentJSONQuery = (HT.hContentType, "application/query+json")

acceptJSON :: HT.Header
acceptJSON = (HT.hAccept, "application/json")

isQuery :: HT.Header
isQuery = ("x-ms-documentdb-isquery", "True")

isUpsert :: HT.Header
isUpsert = ("x-ms-documentdb-is-upsert", "True")

maxItemCount :: HT.HeaderName
maxItemCount = "x-ms-max-item-count"

continuation :: HT.HeaderName
continuation = "x-ms-continuation"

queryCrossPartition :: HT.HeaderName
queryCrossPartition = "x-ms-documentdb-query-enablecrosspartition"

version :: HT.Header
version = ("x-ms-version", "2016-07-11")

sessionToken :: HT.HeaderName
sessionToken = "x-ms-session-token"

msDate :: HT.HeaderName
msDate = "x-ms-date"

-- | Get or set a named header
-- header' :: Eq k => k -> Lens' [(k, v)] (Maybe v)
header' :: (Functor f, Eq a)
  => a
  -> (Maybe b -> f (Maybe b))
  -> [(a, b)]
  -> f [(a, b)]
header' name = lens (lookup name) setOrRemove
  where
    setOrRemove lst =
      ($ remove lst)  -- apply to filtered list (remove old value)
      . maybe id (:)  -- either no change or cons
      . fmap (name, ) -- make key + value pair
    remove = filter ((/= name) . fst)
