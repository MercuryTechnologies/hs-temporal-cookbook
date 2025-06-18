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
  [ ("fr", Map.fromList [("hello", "bonjour"), ("goodbye", "au revoir"), ("thanks", "merci")])
  , ("es", Map.fromList [("hello", "hola"), ("goodbye", "adiós"), ("thanks", "gracias")])
  , ("de", Map.fromList [("hello", "hallo"), ("goodbye", "auf wiedersehen"), ("thanks", "danke schön")])
  , ("pt", Map.fromList [("hello", "olá"), ("goodbye", "tchau"), ("thanks", "obrigado")])
  , ("lv", Map.fromList [("hello", "sveiks"), ("goodbye", "ardievu"), ("thanks", "paldies")])
  , ("mi", Map.fromList [("hello", "kia ora"), ("goodbye", "poroporoaki"), ("thanks", "whakawhetai koe")])
  , ("sk", Map.fromList [("hello", "ahoj"), ("goodbye", "zbohom"), ("thanks", "ďakujem")])
  , ("tr", Map.fromList [("hello", "merhaba"), ("goodbye", "güle güle"), ("thanks", "teşekkür ederim")])
  , ("zu", Map.fromList [("hello", "sawubona"), ("goodbye", "hamba kahle"), ("thanks", "ngiyabonga")])
  ]

main :: IO ()
main = do
  putStrLn "Starting Translation Service..."
  putStrLn ""
  putStrLn "Supported languages: fr, es, de, pt, lv, mi, sk, tr, zu"
  putStrLn ""
  
  scotty 9001 $ do
    -- Exercise1
    get "/translate/:term/:lang" $ do
      term <- pathParam "term"
      lang <- pathParam "lang"
      
      liftIO . T.IO.putStrLn $ "[Exercise1] Translating '" <> term <> "' to " <> lang
      translateAndRespond term lang
    
    -- UsingRecordTypes
    get "/translate" $ do
      term <- queryParam "term"
      lang <- queryParam "lang"
      
      liftIO . T.IO.putStrLn $ "[UsingRecordTypes] Translating '" <> term <> "' to " <> lang
      translateAndRespond term lang
    
    get "/health" $ do
      text "Translation service is running"

translateAndRespond :: Text -> Text -> ActionM ()
translateAndRespond term lang = do
  case Map.lookup (T.toLower lang) translations of
    Nothing -> do
      liftIO . T.IO.putStrLn $ "-> Language '" <> lang <> "' not supported"
      status status404
      text $ "Language '" <> TL.fromStrict lang <> "' not supported"
      finish
    Just termMap ->
      case Map.lookup (T.toLower term) termMap of
        Just translation -> do
          let finalTranslation = if T.take 1 term == T.toUpper (T.take 1 term)
                then T.toUpper (T.take 1 translation) <> T.drop 1 translation
                else translation
          liftIO . T.IO.putStrLn $ "-> " <> finalTranslation
          text $ TL.fromStrict finalTranslation
        Nothing -> do
          liftIO . T.IO.putStrLn $ "-> Term '" <> term <> "' not found in " <> lang
          status status404
          text $ "Unable to translate '" <> TL.fromStrict term <> "' to '" <> TL.fromStrict lang <> "'"
          finish