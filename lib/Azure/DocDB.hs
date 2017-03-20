
module Azure.DocDB (
  module Azure.DocDB.DocumentStore,

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

import Azure.DocDB.DocumentStore

import Azure.DocDB.ETag (
  ETag(..),
  ETagged(..)
  )
import Azure.DocDB.DBResourceId (
  CollectionId(..),
  DocumentId(..),
  ExtendPath(..),
  )
import Azure.DocDB.DBSocketMonad (
  DBSocketState,
  DBSocketMonad,
  DBSocketT,
  DBError(..),
  execDBSocketT,
  mkDBSocketState,
  )
