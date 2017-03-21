
module Azure.DocDB (
  module Azure.DocDB.Store,
  module Azure.DocDB.SocketMonad,

  ETag(..),
  ETagged(..),
  ProvideETag(..),

  CollectionId(..),
  DocumentId(..),
  ExtendPath(..),

  ) where

import Azure.DocDB.Store

import Azure.DocDB.ETag (
  ETag(..),
  ETagged(..),
  ProvideETag(..),
  )
import Azure.DocDB.ResourceId (
  CollectionId(..),
  DocumentId(..),
  ExtendPath(..),
  )

import Azure.DocDB.SocketMonad
