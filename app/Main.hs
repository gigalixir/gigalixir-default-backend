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
import Text.Mustache
import qualified Data.Text.Lazy as TL
import Data.Aeson

main :: IO ()
main = do
  template <- compileMustacheDir "index" "views/"
  -- TODO: EmptySession? defaultCfg sets a cookie
  spockCfg <- defaultSpockCfg () PCNoDatabase ()
  port <- getPort
  apiKey <- getApiKey
  -- how to best thread env vars through the code?
  -- here we just pass params all the way down
  -- is it better to use a Reader monad?
  -- what about the ActionCtxT? does it have something
  -- we can use? AppState appropriate?
  runSpock port (spock spockCfg $ app apiKey template)

getPort :: IO Int
getPort = do
  port <- lookupEnv "PORT"
  return $ fromMaybe 8080 $ (readMaybe $ fromMaybe "" $ port :: Maybe Int)

getApiKey :: IO Text
getApiKey = do
  apiKey <- lookupEnv "APIKEY"
  return $ fromMaybe "" $ fmap pack apiKey

app :: ApiKey -> Template -> SpockM () () () ()
app apiKey template = do
  get root      $ handleRoot apiKey template
  get "healthz" $ text "ok"
  get wildcard $ \_ -> handleRoot apiKey template

handleRoot :: MonadIO m => ApiKey -> Template -> ActionCtxT ctx m a
handleRoot apiKey template = do
  -- TODO: strip the port if it exists
  maybeHost <- header "Host"
  status <- liftIO . (domainStatus apiKey). Domain $ fromMaybe "" maybeHost
  _ <- setStatus notFound404
  renderStatus template (Domain $ fromMaybe "" maybeHost) status

renderStatus :: MonadIO m => Template -> Domain -> Types.Status -> ActionCtxT ctx m a
renderStatus template (Domain domain) DomainNotFound =
  html (render template "domain_not_found" (object ["domain" .= domain]))
renderStatus _ _ status = text . fromString . show $ status

render :: Template -> PName -> Value -> Text
render template pname = TL.toStrict . renderMustache (template {templateActual = pname})
