{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Network.HTTP.Types.Status
import Web.Scotty
import qualified Data.Map.Strict as Map

-- | Translation dictionary
translations :: Map.Map Text (Map.Map Text Text)
translations = Map.fromList
  [ ("fr", Map.fromList [("hello", "bonjour"), ("goodbye", "au revoir")])
  , ("es", Map.fromList [("hello", "hola"), ("goodbye", "adiós")])
  , ("de", Map.fromList [("hello", "hallo"), ("goodbye", "auf wiedersehen")])
  , ("pt", Map.fromList [("hello", "olá"), ("goodbye", "tchau")])
  ]

main :: IO ()
main = do
  scotty 9001 $ do
    get "/translate/:term/:lang" $ do
      term <- pathParam "term"
      lang <- pathParam "lang"
      
      liftIO $ putStrLn $ "Translating '" ++ T.unpack term ++ "' to " ++ T.unpack lang
      
      case Map.lookup lang translations >>= Map.lookup (T.toLower term) of
        Just translation -> do
          liftIO $ putStrLn $ "-> " ++ T.unpack translation
          text $ TL.fromStrict translation
        Nothing -> do
          liftIO $ putStrLn $ "-> Translation not found,"
          text $ TL.fromStrict term