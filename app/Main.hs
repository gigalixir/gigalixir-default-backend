{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Spock.Core 
import Network.Wai (Middleware)

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (lift)
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
import Text.Regex

main :: IO ()
main = do
  port <- getPort
  -- how to best thread env vars through the code?
  -- here we just pass params all the way down
  -- is it better to use a Reader monad?
  -- what about the ActionCtxT? does it have something
  -- we can use? AppState appropriate?
  runSpock port app

app :: IO Middleware
app = do
  template <- compileMustacheDir "index" "views/"
  -- TODO: EmptySession? defaultCfg sets a cookie
  -- spockCfg <- defaultSpockCfg () PCNoDatabase ()
  apiKey <- getApiKey
  -- (spock spockCfg $ routes apiKey template)
  spockT id $ routes apiKey template

getPort :: IO Int
getPort = do
  port <- lookupEnv "PORT"
  return $ fromMaybe 8080 $ (readMaybe $ fromMaybe "" $ port :: Maybe Int)

getApiKey :: IO Text
getApiKey = do
  apiKey <- lookupEnv "APIKEY"
  return $ fromMaybe "" $ fmap pack apiKey

routes :: (GigalixirApiM m, MonadIO m) => ApiKey -> Template -> SpockCtxT ctx m ()
routes apiKey template = do
  get root      $ handleRoot apiKey template
  get "healthz" $ text "ok"
  get wildcard  $ \_ -> handleRoot apiKey template

handleRoot :: (GigalixirApiM m, MonadIO m) => ApiKey -> Template -> ActionCtxT ctx m a
handleRoot apiKey template = do
  -- TODO: strip the port if it exists
  maybeXCode <- header "X-Code"
  handleRootWithXCode maybeXCode apiKey template

handleRootWithXCode :: (GigalixirApiM m, MonadIO m) => Maybe Text -> ApiKey -> Template -> ActionCtxT ctx m a
handleRootWithXCode Nothing apiKey template = do
  _ <- setStatus notFound404
  maybeHost <- header "Host"
  let domain = Domain $ fromMaybe "" maybeHost in do
    status <- lift $ (domainStatus apiKey) $ domain 
    renderStatus template domain status
handleRootWithXCode (Just "504") _apiKey template = do
  _ <- setStatus gatewayTimeout504
  maybeHost <- header "Host"
  let domain = Domain $ fromMaybe "" maybeHost in do
    -- domain is not really used?
    renderStatus template domain ReleaseUnhealthy
handleRootWithXCode (Just _) apiKey template = handleRootWithXCode Nothing apiKey template

renderStatus :: MonadIO m => Template -> Domain -> Types.Status -> ActionCtxT ctx m a
renderStatus template (Domain domain) DomainNotFound =
  html (render template "domain_not_found" (object ["domain" .= domain]))
renderStatus template (Domain domain) AppNotFound =
  -- regex from web/models/app.ex
  let app_name = matchRegex (mkRegex "^([a-zA-Z0-9_-]+)[.]gigalixirapp[.]com$") (unpack domain) in
    case app_name of
      Just [a] ->
        html (render template "app_not_found" (object ["app_name" .= a]))
      _ ->
        text "Problem finding app name. Please contact us at help@gigalixir.com."
renderStatus template (Domain domain) ReleaseUnhealthy =
  html (render template "release_unhealthy" (object ["domain" .= domain]))
renderStatus template (Domain domain) ReleaseNotFound =
  html (render template "release_not_found" (object ["domain" .= domain]))
renderStatus template (Domain domain) ReplicasNotFound =
  html (render template "replicas_not_found" (object ["domain" .= domain]))
renderStatus template (Domain domain) Ok =
  html (render template "ok" (object ["domain" .= domain]))
renderStatus _ (Domain domain) Types.Error = text (domain <> ":" <> (fromString $ show Types.Error))

render :: Template -> PName -> Value -> Text
render template pname = TL.toStrict . renderMustache (template {templateActual = pname})


