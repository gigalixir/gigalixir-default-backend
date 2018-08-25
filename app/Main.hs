{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Spock
import Web.Spock.Config

import Control.Monad.IO.Class
import Data.String
import Data.Maybe
import System.Environment
import Text.Read (readMaybe)
import Api
import Types
import Data.Text

main :: IO ()
main = do
  spockCfg <- defaultSpockCfg () PCNoDatabase ()
  port <- getPort
  apiKey <- getApiKey
  print apiKey
  -- how to thread env vars through the code?
  runSpock port (spock spockCfg $ app apiKey)

getPort :: IO Int
getPort = do
  port <- lookupEnv "PORT"
  return $ fromMaybe 8080 $ (readMaybe $ fromMaybe "" $ port :: Maybe Int)

getApiKey :: IO Text
getApiKey = do
  apiKey <- lookupEnv "APIKEY"
  return $ fromMaybe "" $ fmap pack apiKey

app :: ApiKey -> SpockM () () () ()
app apiKey = do
  get root      $ handleRoot apiKey
  get "healthz" $ text "ok"

handleRoot :: MonadIO m => ApiKey -> ActionCtxT ctx m a
handleRoot apiKey = do
  maybeHost <- header "Host"
  status <- liftIO . (domainStatus apiKey). Domain $ fromMaybe "" maybeHost
  -- TODO: render a template
  text . fromString . show $ status
