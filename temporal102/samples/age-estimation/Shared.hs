module Shared where

-- NOTE: Required to import workflow instances that this worker will handle.
import Workflow ()

import Control.Monad.Logger (defaultOutput)
import Control.Monad.IO.Class (MonadIO)
import Data.Default (def)
import Data.Functor ((<&>))
import DiscoverInstances (discoverInstances)
import Network.Connection (settingClientSupported)
import Network.HTTP.Client qualified as HTTP.Client
import Network.HTTP.Client.TLS (mkManagerSettings, newTlsManagerWith)
import qualified Network.TLS as TLS
import OpenTelemetry.Trace (Tracer, getGlobalTracerProvider, makeTracer, tracerOptions)
import RequireCallStack (RequireCallStack, provideCallStack)
import System.IO (stdout)
import Temporal.Interceptor (Interceptors)
import Temporal.TH (WorkflowFn, ActivityFn)
import Temporal.TH qualified
import Temporal.Worker (WorkerConfig)
import Temporal.Worker qualified as Worker
import Temporal.Workflow qualified as Workflow

mkWorkerConfig :: forall env. env ~ HTTP.Client.Manager => Interceptors env -> env -> WorkerConfig env
mkWorkerConfig interceptors manager = provideCallStack $ Worker.configure manager definitions settings
  where
    definitions :: RequireCallStack => Worker.Definitions env
    definitions = Temporal.TH.discoverDefinitions $$(discoverInstances) $$(discoverInstances)
    settings = do
      Worker.setNamespace namespace
      Worker.setTaskQueue taskQueue
      Worker.setLogger (defaultOutput stdout)
      Worker.addInterceptors interceptors

namespace :: Workflow.Namespace
namespace = "default"

taskQueue :: Workflow.TaskQueue
taskQueue = "test"

-- | The third-party service used to implement this example supports TLS, but
-- does /not/ support TLS1.2+EMS (Extended Main Secret).
--
-- The 'http-client-tls' connection manager has 'TLS.RequireEMS' set by
-- default, which causes requests to fail.
--
-- Fortunately this makes for a useful example of how we can use the 'Activity'
-- environment to as a form of dependency injection.
newRelaxedEMSManager :: MonadIO m => m HTTP.Client.Manager
newRelaxedEMSManager = do
  let tlsSettings = def {
        settingClientSupported = def {
          TLS.supportedExtendedMainSecret = TLS.AllowEMS
        }
      }
  newTlsManagerWith $ mkManagerSettings tlsSettings Nothing

