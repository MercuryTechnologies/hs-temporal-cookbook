module Workflow where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import GHC.Generics (Generic)
import Network.HTTP.Simple (httpBS, getResponseBody, parseRequest)
import RequireCallStack (provideCallStack)
import Temporal.Activity (Activity)
import Temporal.Duration (seconds)
import Temporal.TH qualified
import Temporal.Workflow (Workflow)
import Temporal.Workflow qualified as Workflow

-- | Input data for the hello/goodbye translation workflow
data TranslationInput = TranslationInput
  { name :: Text           -- Name to greet
  , languageCode :: Text   -- Target language (fr, es, de, pt)
  } deriving (Generic, FromJSON, ToJSON, Show)

-- | Output data containing translated greetings
data TranslationOutput = TranslationOutput
  { helloMessage :: Text     -- Translated hello message
  , goodbyeMessage :: Text   -- Translated goodbye message
  } deriving (Generic, FromJSON, ToJSON, Show)

-- | Activity that translates by calling the translation service
translateActivity :: (Text, Text) -> Activity () Text
translateActivity (term, lang) = do
  req <- parseRequest $ "http://localhost:9001/translate/" ++ T.unpack term ++ "/" ++ T.unpack lang
  resp <- httpBS req
  pure . decodeUtf8 $ getResponseBody resp

-- Register the activity with Temporal's code generation
Temporal.TH.registerActivity 'translateActivity

-- | Workflow configuration options for activities
activityOptions :: Workflow.StartActivityOptions
activityOptions =
  Workflow.defaultStartActivityOptions (Workflow.StartToClose $ seconds 30)

-- | Main workflow that demonstrates durable execution
sayHelloGoodbyeWorkflow :: TranslationInput -> Workflow TranslationOutput
sayHelloGoodbyeWorkflow input = provideCallStack $ do
  -- First activity: translate "hello"
  hello <- Workflow.executeActivity TranslateActivity activityOptions ("hello", input.languageCode)
  let helloMsg = hello <> ", " <> input.name
  
  -- 10-second sleep: perfect time to test worker failure recovery
  Workflow.sleep (seconds 10)
  
  -- Second activity: translate "goodbye" 
  goodbye <- Workflow.executeActivity TranslateActivity activityOptions ("goodbye", input.languageCode)
  let goodbyeMsg = goodbye <> ", " <> input.name
  
  pure $ TranslationOutput helloMsg goodbyeMsg

-- Register the workflow with Temporal's code generation
Temporal.TH.registerWorkflow 'sayHelloGoodbyeWorkflow
