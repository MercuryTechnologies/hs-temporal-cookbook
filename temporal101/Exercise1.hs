module Main where

import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (defaultOutput, runStdoutLoggingT)
import DiscoverInstances (discoverInstances)
import RequireCallStack (RequireCallStack, provideCallStack)
-- SayHello imports our workflow definition
import SayHello
import System.IO (stdout)
import Temporal.Client (mkWorkflowClientConfig, workflowClient)
import Temporal.Core.Client (connectClient, defaultClientConfig)
import Temporal.Runtime (TelemetryOptions (..), initializeRuntime)
-- WorkflowFn and ActivityFn are used implicitly in the
-- discoverInstances calls in workerConfig
import Temporal.TH (WorkflowFn, ActivityFn)
import Temporal.TH qualified
import Temporal.Worker (Worker, WorkerConfig, startWorker)
import Temporal.Worker qualified as Worker
import Temporal.Workflow qualified as Workflow
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (bracket)

taskQueue :: Workflow.TaskQueue
taskQueue = "hello-world"

namespace :: Workflow.Namespace
namespace = "default"

workerConfig :: WorkerConfig ()
workerConfig = provideCallStack $ Worker.configure environment definitions settings
  where
    -- workers are configured with an environment, which is made
    -- available to activities at execution-time.
    --
    -- the workflow in this exercise has no activities, and so we
    -- configure its workers with an empty environment (that is, '()').
    --
    -- in a more complex deployment, workers would configure database
    -- connection pools, shared secrets, etc. at initialization so that
    -- activities could make use of them.
    environment = ()
    -- the template haskell here is guided by the type of the
    -- environment we provide; here, it's @()@ again. we will only
    -- discover definitions that are defined on this particular
    -- environment type.
    definitions :: RequireCallStack => Worker.Definitions ()
    definitions = Temporal.TH.discoverDefinitions $$(discoverInstances) $$(discoverInstances)
    settings = do
      Worker.setNamespace namespace
      Worker.setTaskQueue taskQueue
      Worker.setLogger (defaultOutput stdout)

main :: IO ()
main = bracket setup teardown $ \_worker -> do
  -- the worker's running in its own thread, we just have to keep this
  -- one alive jfor as long as we want to execute workflows on it
  forever $ threadDelay maxBound
  where
    setup = do
      runtime <- initializeRuntime NoTelemetry
      -- coreClient is needed to construct a worker, even though we're
      -- not constructing a client
      coreClient <- runStdoutLoggingT $ connectClient runtime defaultClientConfig

      -- startWorker starts a Temporal worker and returns a handle to
      -- it. the Temporal control plane can dispatch to the worker, or
      -- you can run workflows on it directly
      worker <- startWorker coreClient workerConfig

      pure worker

    teardown worker = Worker.shutdown worker
