{-# LANGUAGE OverloadedStrings #-}

module Azure.DocDB.ServiceHeader where


import qualified Network.HTTP.Client as HC
import qualified Network.HTTP.Types.Header as HT


contentJSON :: HT.Header
contentJSON = (HT.hContentType, "application/json")

contentJSONQuery :: HT.Header
contentJSONQuery = (HT.hContentType, "application/query+json")

acceptJSON :: HT.Header
acceptJSON = (HT.hAccept, "application/json")

isQuery :: HT.Header
isQuery = ("x-ms-documentdb-isquery", "True")

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


-- | Modify headers on a request
modifyHeaders :: ([HT.Header] -> [HT.Header]) -> HC.Request -> HC.Request
modifyHeaders f req =
  req { HC.requestHeaders = f . HC.requestHeaders $ req }

-- | Prepend a header to a request
addHeader :: HT.Header -> HC.Request -> HC.Request
addHeader h = modifyHeaders ((:) h)
