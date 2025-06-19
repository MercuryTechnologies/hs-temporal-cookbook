{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.IO.Utf8 qualified as T.IO
import Network.HTTP.Types.Status
import Web.Scotty
import qualified Data.Map.Strict as Map

-- | Translation dictionary
translations :: Map.Map Text (Map.Map Text Text)
translations = Map.fromList
  [ ("de", Map.fromList [("hello", "hallo"), ("goodbye", "auf wiedersehen"), ("thanks", "danke schon")])
  , ("fr", Map.fromList [("hello", "bonjour"), ("goodbye", "au revoir"), ("thanks", "merci")])
  , ("es", Map.fromList [("hello", "hola"), ("goodbye", "adiós"), ("thanks", "gracias")])
  , ("lv", Map.fromList [("hello", "sveiks") , ("goodbye", "ardievu") , ("thanks", "paldies") ])
  , ("mi", Map.fromList [("hello", "kia ora") , ("goodbye", "poroporoaki") , ("thanks", "whakawhetai koe") ])
  , ("sk", Map.fromList [("hello", "ahoj") , ("goodbye", "zbohom") , ("thanks", "ďakujem koe") ])
  , ("tr", Map.fromList [("hello", "merhaba") , ("goodbye", "güle güle") , ("thanks", "teşekkür ederim") ])
  , ("zu", Map.fromList [("hello", "hamba kahle") , ("goodbye", "sawubona") , ("thanks", "ngiyabonga") ])
  ]

main :: IO ()
main = do
  putStrLn "Starting Translation Service..."
  
  scotty 9001 $ do
    get "/translate/:term/:lang" $ do
      term <- pathParam "term"
      lang <- pathParam "lang"
      
      liftIO . T.IO.putStrLn $ "Translating '" <> term <> "' to " <> lang
      
      case Map.lookup lang translations of
        Nothing -> do
          liftIO . T.IO.putStrLn $ "-> Language '" <> lang <> "' not supported"
          status status400
          text $ "Language '" <> TL.fromStrict lang <> "' not supported."
          finish
        Just termMap ->
          case Map.lookup (T.toLower term) termMap of
            Just translation -> do
              liftIO . T.IO.putStrLn $ "-> " <> translation
              text $ TL.fromStrict translation
            Nothing -> do
              liftIO . T.IO.putStrLn $ "-> Term '" <> term <> "' not found in " <> lang
              status status400
              text $ "Unable to translate '" <> TL.fromStrict term <> "' to '" <> TL.fromStrict lang <> "'."
              finish
    
    get "/health" $ do
      text "Translation service is running"
