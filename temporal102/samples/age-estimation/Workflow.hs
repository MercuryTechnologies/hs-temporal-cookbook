module Workflow where

import Control.Exception (throwIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Data.Aeson (FromJSON, ToJSON)
import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics (Generic)
import Network.HTTP.Client qualified as HTTP.Client
import OpenTelemetry.Instrumentation.HttpClient.Simple (
    getResponseBody,
    httpJSON,
    parseRequest,
    setRequestManager,
    setRequestQueryString
  )
import RequireCallStack (provideCallStack)
import Temporal.Activity (Activity)
import Temporal.Duration (seconds)
import Temporal.Exception (ApplicationFailure (..))
import Temporal.TH qualified
import Temporal.Workflow (Workflow)
import Temporal.Workflow qualified as Workflow

-- | Helper function to render any 'Show'-able type as 'Text'.
tshow :: Show a => a -> Text
tshow = Text.pack . show

data EstimatorResponse = EstimatorResponse
  { age :: Int
  , count :: Int
  , name :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)

estimateAge :: Text -> Activity HTTP.Client.Manager Int
estimateAge name = do
  manager <- ask
  req <- parseRequest "https://api.agify.io/"
    <&> setRequestQueryString [("name", Just . encodeUtf8 $ name)]
    <&> setRequestManager manager
  EstimatorResponse {age} <- httpJSON mempty req <&> getResponseBody
  pure age

-- Register the activity with Temporal's code generation
Temporal.TH.registerActivity 'estimateAge

-- | Workflow configuration options for activities
activityOptions :: Workflow.StartActivityOptions
activityOptions =
  Workflow.defaultStartActivityOptions (Workflow.StartToClose $ seconds 30)

estimateAgeWorkflow :: Text -> Workflow Text
estimateAgeWorkflow name = provideCallStack $ do
  age <- Workflow.executeActivity EstimateAge activityOptions name
  pure $ name <> " has an estimated age of " <> tshow age

Temporal.TH.registerWorkflow 'estimateAgeWorkflow
