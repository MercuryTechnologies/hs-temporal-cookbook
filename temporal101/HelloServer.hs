import Data.IORef
import Network.HTTP.Types.Status
import Web.Scotty

main :: IO ()
main = do
  attempts <- newIORef 0
  scotty 9001 $ do
    get "/hello/:name" $ do
      name <- pathParam "name"
      text $ "¡Hola, " <> name <> "!"
    get "/goodbye/:name" $ do
      name <- pathParam "name"
      text $ "¡Adios, " <> name <> "!"
    get "/sometimesBusy" $ do
      liftIO $ modifyIORef' attempts (+1)
      attempt <- liftIO $ readIORef attempts
      if attempt < 3 then failureResponse else successResponse
      where
        failureResponse = do
          status status429
          text "please call back later"
        successResponse = text "thank you, drive through"
