# Azure.DocDB

DocDB document access in Haskell.  For example:


```haskell
import           Azure.DocDB
import           Control.Monad.IO.Class
import           Data.Aeson ((.=))
import qualified Data.Aeson as A
import qualified Data.HashMap.Lazy as HM

key = "{base64-encoded-key}"
accountEndpoint = "https://{databaseaccount}.documents.azure.com"
docToMake = "myDB" #> "myCollection" #> "myDoc"

main :: IO ()
  manager <- newManager tlsManagerSettings
  state <- mkDBSocketState key accountEndpoint manager

  execDBSocketT (testCreateDoc docToMake) state

testCreateDoc :: (MonadIO m, DBSocketMonad m) => DocumentId -> m (ETagged A.Value)
testCreateDoc (DocumentId collection docName) = do
  let testDoc = A.Object $ HM.fromList
                  [ ("id", A.String docName),
                    ("Hello", A.Number 98000) ]
  result :: ETagged A.Value <- createDocument collection testDoc
  liftIO $ print result
  return result
```
