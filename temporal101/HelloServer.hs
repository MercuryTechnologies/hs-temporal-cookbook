import Web.Scotty

main :: IO ()
main = scotty 9001 $ do
  get "/hello/:name" $ do
    name <- pathParam "name"
    text $ "¡Hola, " <> name <> "!"
  get "/goodbye/:name" $ do
    name <- pathParam "name"
    text $ "¡Adios, " <> name <> "!"
