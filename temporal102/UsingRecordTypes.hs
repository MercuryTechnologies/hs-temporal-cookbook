module UsingRecordTypes where

import Exercise1 (taskQueue, namespace, workerConfig, runWorker)
import qualified Exercise1
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import GHC.Generics (Generic)
import Network.HTTP.Simple (httpBS, getResponseBody, parseRequest)
import Network.HTTP.Types (urlEncode)
import RequireCallStack (provideCallStack)
import Temporal.Activity (Activity)
import Temporal.Duration (seconds)
import Temporal.TH qualified
import Temporal.Workflow (Workflow)
import Temporal.Workflow qualified as Workflow

type TranslationWorkflowInput = Exercise1.TranslationInput
type TranslationWorkflowOutput = Exercise1.TranslationOutput

-- | Input record for translation activities
data TranslationActivityInput = TranslationActivityInput
  { term :: Text           -- The term to translate (hello, goodbye, etc.)
  , languageCode :: Text   -- Target language code (fr, es, de, etc.)
  } deriving (Generic, FromJSON, ToJSON, Show)

-- | Output record for translation activities
data TranslationActivityOutput = TranslationActivityOutput
  { translation :: Text    -- The translated term
  } deriving (Generic, FromJSON, ToJSON, Show)

-- | Activity that translates terms using input/output
translateTermActivity :: TranslationActivityInput -> Activity () TranslationActivityOutput
translateTermActivity input = do
  let lang = T.unpack $ decodeUtf8 $ urlEncode True $ encodeUtf8 input.languageCode
      term = T.unpack $ decodeUtf8 $ urlEncode True $ encodeUtf8 input.term
      url = "http://localhost:9001/translate?lang=" ++ lang ++ "&term=" ++ term
  
  req <- parseRequest url
  resp <- httpBS req
  let translatedText = T.strip $ decodeUtf8 $ getResponseBody resp
  
  pure $ TranslationActivityOutput translatedText

-- Register the activity with Temporal
Temporal.TH.registerActivity 'translateTermActivity

activityOptions :: Workflow.StartActivityOptions
activityOptions =
  Workflow.defaultStartActivityOptions (Workflow.StartToClose $ seconds 45)

-- | Main workflow demonstrating structured record usage
sayHelloGoodbyeWorkflowWithRecords :: TranslationWorkflowInput -> Workflow TranslationWorkflowOutput
sayHelloGoodbyeWorkflowWithRecords input = provideCallStack $ do
  -- Create structured input records for each translation
  let helloInput = TranslationActivityInput 
        { term = "Hello"
        , languageCode = input.languageCode
        }
  
  let goodbyeInput = TranslationActivityInput
        { term = "Goodbye" 
        , languageCode = input.languageCode
        }
  
  helloResult <- Workflow.executeActivity TranslateTermActivity activityOptions helloInput
  goodbyeResult <- Workflow.executeActivity TranslateTermActivity activityOptions goodbyeInput
  
  let helloMessage = helloResult.translation <> ", " <> input.name
      goodbyeMessage = goodbyeResult.translation <> ", " <> input.name
  
  pure $ Exercise1.TranslationOutput helloMessage goodbyeMessage

Temporal.TH.registerWorkflow 'sayHelloGoodbyeWorkflowWithRecords

main :: IO ()
main = Exercise1.runWorker