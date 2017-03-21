
module Azure.DocDB (
  module Azure.DocDB.Store,
  module Azure.DocDB.SocketMonad,

  ETag(..),
  ETagged(..),

  CollectionId(..),
  DocumentId(..),
  ExtendPath(..),

  ) where

import Azure.DocDB.Store

import Azure.DocDB.ETag (
  ETag(..),
  ETagged(..),
  )
import Azure.DocDB.ResourceId (
  CollectionId(..),
  DocumentId(..),
  ExtendPath(..),
  )

import Azure.DocDB.SocketMonad
