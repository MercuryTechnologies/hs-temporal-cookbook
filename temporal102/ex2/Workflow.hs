module Workflow where

import Control.Exception (throw)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import GHC.Generics (Generic)
import Network.HTTP.Simple (httpBS, getResponseBody, getResponseStatusCode, parseRequest)
import RequireCallStack (provideCallStack)
import Temporal.Activity (Activity)
import Temporal.Duration (seconds)
import Temporal.Exception (ApplicationFailure (..))
import Temporal.TH qualified
import Temporal.Workflow (Workflow)
import Temporal.Workflow qualified as Workflow

tshow :: Show a => a -> Text
tshow = T.pack . show

data TranslateTermInput = TranslateTermInput
  { term :: Text
  , lang :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)

translateActivity :: TranslateTermInput -> Activity () Text
translateActivity TranslateTermInput{..} = do
  req <- parseRequest $ "http://localhost:9001/translate/" ++ T.unpack term ++ "/" ++ T.unpack lang
  resp <- httpBS req
  let body = decodeUtf8 $ getResponseBody resp
  let status = getResponseStatusCode resp
  case status of
    200 -> pure body
    _ -> reportTranslationError status body
  where
    reportTranslationError err body = throw
      ApplicationFailure
        { type' = "TranslationError"
        , message = "Status " <> tshow err <> ": " <> tshow body
        , nonRetryable = True
        , details = []
        , stack = ""
        , nextRetryDelay = Nothing
        }

-- Register the activity with Temporal's code generation
Temporal.TH.registerActivity 'translateActivity

-- | Workflow configuration options for activities
activityOptions :: Workflow.StartActivityOptions
activityOptions =
  Workflow.defaultStartActivityOptions (Workflow.StartToClose $ seconds 30)

data TranslationInput = TranslationInput
  { name :: Text
  , languageCode :: Text
  } 
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data TranslationOutput = TranslationOutput
  { helloMessage :: Text
  , goodbyeMessage :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

sayHelloGoodbyeWorkflow :: TranslationInput -> Workflow TranslationOutput
sayHelloGoodbyeWorkflow input = provideCallStack $ do
  hello <- Workflow.executeActivity TranslateActivity activityOptions $ TranslateTermInput "hello" input.languageCode
  let helloMsg = hello <> ", " <> input.name
  
  Workflow.sleep (seconds 10)
  
  goodbye <- Workflow.executeActivity TranslateActivity activityOptions $ TranslateTermInput "goodbye" input.languageCode
  let goodbyeMsg = goodbye <> ", " <> input.name
  
  pure $ TranslationOutput helloMsg goodbyeMsg

Temporal.TH.registerWorkflow 'sayHelloGoodbyeWorkflow
