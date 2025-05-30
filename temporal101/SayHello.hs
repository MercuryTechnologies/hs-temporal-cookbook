module SayHello where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text
import GHC.Generics (Generic)
import RequireCallStack (RequireCallStack, provideCallStack)
import Temporal.TH (WorkflowFn)
import Temporal.TH qualified
import Temporal.Workflow (Workflow, WorkflowId (..))

-- | The official "Hello, Workflow" example takes a bare string as input.
-- Here we reinforce the pattern of creating a record type for inputs so
-- that it can evolve freely.
data SayHelloInput = SayHelloInput
  { name :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

-- | We don't execute an activity in this workflow, we execute code
-- directly (why?).
sayHelloWorkflow :: SayHelloInput -> Workflow Text
sayHelloWorkflow input = provideCallStack do
  pure $ "Hello, " <> input.name

Temporal.TH.registerWorkflow 'sayHelloWorkflow
-- Temporal.TH.bringRegisteredTemporalFunctionsIntoScope
