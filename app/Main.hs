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
import Network.HTTP.Types.Status

main :: IO ()
main = do
  -- TODO: EmptySession? defaultCfg sets a cookie
  spockCfg <- defaultSpockCfg () PCNoDatabase ()
  port <- getPort
  apiKey <- getApiKey
  -- how to best thread env vars through the code?
  -- here we just pass params all the way down
  -- is it better to use a Reader monad?
  -- what about the ActionCtxT? does it have something
  -- we can use? AppState appropriate?
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
  get wildcard $ \_ -> handleRoot apiKey

handleRoot :: MonadIO m => ApiKey -> ActionCtxT ctx m a
handleRoot apiKey = do
  maybeHost <- header "Host"
  status <- liftIO . (domainStatus apiKey). Domain $ fromMaybe "" maybeHost
  -- TODO: render a template
  _ <- setStatus notFound404
  text . fromString . show $ status
