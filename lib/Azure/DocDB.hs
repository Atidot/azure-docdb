
module Azure.DocDB (
  module Azure.DocDB.Store,

  ETag(..),
  ETagged(..),

  CollectionId(..),
  DocumentId(..),
  ExtendPath(..),

  DBSocketState,
  DBSocketMonad,
  DBSocketT,
  DBError(..),
  execDBSocketT,
  mkDBSocketState
  ) where

import Azure.DocDB.Store

import Azure.DocDB.ETag (
  ETag(..),
  ETagged(..)
  )
import Azure.DocDB.ResourceId (
  CollectionId(..),
  DocumentId(..),
  ExtendPath(..),
  )
import Azure.DocDB.SocketMonad (
  DBSocketState,
  DBSocketMonad,
  DBSocketT,
  DBError(..),
  execDBSocketT,
  mkDBSocketState,
  )
