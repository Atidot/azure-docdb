{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE OverloadedStrings    #-}

module Main where

--

import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Data.Aeson ((.=))
import qualified Data.Aeson as A
import           Data.Conduit (runConduit, awaitForever, Source, (=$), ($$))
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Lazy as HM
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Network.HTTP.Client (newManager, defaultManagerSettings, managerModifyRequest)
import           Network.HTTP.Client.TLS (tlsManagerSettings)

import           Azure.DocDB hiding (collection)
import           Paths_azure_docdb
import           Settings

{-
Database    https://{databaseaccount}.documents.azure.com/dbs/{db}
User 	      https://{databaseaccount}.documents.azure.com/dbs/{db}/users/{user}
Permission 	https://{databaseaccount}.documents.azure.com/dbs/{db}/users/{user}/permissions/{perm}
Collection 	https://{databaseaccount}.documents.azure.com/dbs/{db}/colls/{coll}
SPROC  	    https://{databaseaccount}.documents.azure.com/dbs/{db}/colls/{coll}/sprocs/{sproc}
Trigger 	  https://{databaseaccount}.documents.azure.com/dbs/{db}/colls/{coll}/triggers/{trigger}
UDF 	      https://{databaseaccount}.documents.azure.com/dbs/{db}/colls/{coll}/udfs/{udf}
Document 	  https://{databaseaccount}.documents.azure.com/dbs/{db}/colls/{coll}/docs/{doc}
Attachment 	https://{databaseaccount}.documents.azure.com/dbs/{db}/colls/{coll}/docs/{doc}/attachments/{attch}
Offer 	    https://{databaseaccount}.documents.azure.com/offers/{offer}
-}


--
testGetDoc :: (MonadIO m, DBSocketMonad m) => CollectionId -> m ()
testGetDoc collection = do
  rslt :: Maybe (DBDocument A.Object) <- getDocument (pure (collection #> "testdoc"))
  liftIO $ print rslt


testDeleteDoc :: (MonadIO m, DBSocketMonad m) => DocumentId -> m ()
testDeleteDoc docId = deleteDocument (pure docId)


testCreateDoc :: (MonadIO m, DBSocketMonad m) => DocumentId -> m (DBDocument A.Value)
testCreateDoc (DocumentId collection docName) = do
  let testDoc = A.Object $ HM.fromList
                  [ ("id", A.String docName),
                    ("Hello", A.Number 98000) ]
  rslt2 :: DBDocument A.Value <- createDocument collection testDoc
  liftIO $ print rslt2
  return rslt2


testReplaceDoc :: (MonadIO m, DBSocketMonad m) => ETagged DocumentId -> m (DBDocument A.Value)
testReplaceDoc taggedId@(ETagged tag (DocumentId collection docName)) = do
  let testDoc3 = A.Object $ HM.fromList
                   [ ("id", A.String docName),
                     ("Hello", A.Number 1011) ]
  rslt2 :: DBDocument A.Value <- replaceDocument taggedId testDoc3
  liftIO $ print rslt2
  return rslt2


testListDocs :: (MonadIO m, DBSocketMonad m)
  => CollectionId
  -> m ()
testListDocs collection = runConduit $
  docList =$ awaitForever (liftIO . print)
  where
    docList :: DBSocketMonad m => Source m (DBDocument A.Value)
    docList = listAll $ listDocuments collection



testQueryDocs :: (MonadIO m, DBSocketMonad m)
  => CollectionId
  -> m ()
testQueryDocs collection = docList $$ awaitForever (liftIO . print)
  where
    docList :: DBSocketMonad m => Source m (DBDocument A.Value)
    docList = listAll $ queryDocuments dbQueryParamSimple collection sqlQuery

    sqlQuery = DBSQL "SELECT * FROM Docs d WHERE d.Hello = @h"
      ["@h" .= (1011 :: Int)]


test1 :: (MonadIO m, DBSocketMonad m) => CollectionId -> m ()
test1 collection = do
  sep "testGetDoc"
  testGetDoc collection
  sep "testDeleteDoc"
  safeRun $ testDeleteDoc docId
  sep "testCreateDoc"
  etaggedDoc <- testCreateDoc docId
  sep "testReplaceDoc"
  testReplaceDoc (ETagged (etagOf etaggedDoc) docId)
  sep "query"
  testQueryDocs collection
  sep "testListDocs"
  testListDocs collection

  liftIO $ print "Done Tests"

  where
    docId = collection #> "myTestDoc"
    sep name = liftIO $ do
      putStrLn $ replicate 20 '-'
      putStrLn name
      putStrLn ""
    sqlQuery = DBSQL "SELECT * FROM Docs d WHERE d.Hello = @h"
      ["@h" .= (1011 :: Int)]


safeRun :: (Show a, MonadIO m, MonadError a m) => m () -> m ()
safeRun m = catchError m (
  liftIO . (putStrLn "ERROR CATCH!" *>) . print
  )


main :: IO ()
main = do
  s <- BL.readFile =<< getDataFileName "settings.js"
  settings <- either (fail "No cfg") return (A.eitherDecode s :: Either String Settings)
  let pwd = fromBase64 . secondary . auth $ settings
  let testCollection = db settings #> collection settings

  --
  manager <- newManager tlsManagerSettings
  state <- mkDBSocketState pwd ("https://" `mappend` accountEndpoint settings) manager

  execDBSocketT
    (evalDBSessionT (safeRun $ test1 testCollection))
    state

  print "done"
