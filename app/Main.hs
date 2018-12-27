{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

module Main where

import Web.Spock.Core 
import Network.Wai (Middleware)

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Data.String
import Data.Maybe
-- import Data.Function
-- import Data.ByteString.Lazy.Char8 as C8 (putStrLn)
import qualified Data.ByteString.Lazy as BL
import Network.HTTP.Client.Internal
import System.Environment
-- import Text.Read (readMaybe)
import Api
import Types
import Data.Text
import Network.HTTP.Types.Status
import Text.Mustache
import qualified Data.Text.Lazy as TL
import Data.Aeson
import Text.Regex
import Control.Monad.Trans.Reader
import Network.HTTP.Types.Version (http11)

-- import qualified Control.Lens as L
import qualified Network.Wreq as W

succeededResponse :: Response BL.ByteString
succeededResponse = Response
  { responseStatus = mkStatus 200 "success"
  , responseVersion = http11
  , responseHeaders = []
  , responseBody = "{\"data\":{\"status\":\"ok\"}}"
  , responseCookieJar = createCookieJar []
  , responseClose' = ResponseClose (return () :: IO ())
  }

makeRequest :: W.Options -> String -> (W.Options -> String -> IO a) -> IO a
makeRequest options url requester = requester options url

mockRunner :: MonadIO m => ReaderT (W.Options -> String -> IO (Response BL.ByteString)) m a -> m a
mockRunner r = runReaderT r mockRequester
  where mockRequester _ _ = return succeededResponse

apiResponse :: (MonadIO m) => W.Options -> String -> ReaderT (W.Options -> String -> IO a) m a
apiResponse options url = asks (makeRequest options url) >>= liftIO

main :: IO ()
main = do
  myPort <- getPort
  -- how to best thread env vars through the code?
  -- here we just pass params all the way down
  -- is it better to use a Reader monad?
  -- what about the ActionCtxT? does it have something
  -- we can use? AppState appropriate?
  runSpock myPort app


runner :: MonadIO m => ReaderT (W.Options -> String -> IO (Response BL.ByteString)) m a -> m a
runner r = runReaderT r Api.getWith

app :: IO Middleware
app = do
  template <- compileMustacheDir "index" "views/"
  -- TODO: EmptySession? defaultCfg sets a cookie
  -- spockCfg <- defaultSpockCfg () PCNoDatabase ()
  apiKey <- getApiKey
  -- (spock spockCfg $ routes apiKey template)
  spockT (runner) $ routes apiKey template
  -- spockT (mockRunner) $ routes apiKey template

-- Fetches the PORT environment variable and converts it from a string to an int,
-- and uses 8080 if none was provided
getPort :: IO Int
getPort = do
  myPort <- lookupEnv "PORT"
  return $ read $ fromMaybe "8080" myPort

getApiKey :: IO Text
getApiKey = do
  apiKey <- lookupEnv "APIKEY"
  return $ fromMaybe "" $ fmap pack apiKey

routes :: MonadIO m => ApiKey -> Template -> SpockCtxT ctx (ReaderT (W.Options -> String -> IO (Response BL.ByteString)) m) ()
routes apiKey template = do
  get root      $ handleRoot apiKey template
  get "healthz" $ text "ok"
  get wildcard  $ \_ -> handleRoot apiKey template

handleRoot :: MonadIO m => ApiKey -> Template -> ActionCtxT ctx (ReaderT (W.Options -> String -> IO (Response BL.ByteString)) m) a
handleRoot apiKey template = do
  -- TODO: strip the port if it exists
  maybeXCode <- header "X-Code"
  handleRootWithXCode maybeXCode apiKey template

handleRootWithXCode :: MonadIO m => Maybe Text -> ApiKey -> Template -> ActionCtxT ctx (ReaderT (W.Options -> String -> IO (Response BL.ByteString)) m) a
handleRootWithXCode Nothing apiKey template = do
  _ <- setStatus notFound404
  maybeHost <- header "Host"
  let domain = Domain $ fromMaybe "" maybeHost in do
    -- status <- liftIO $ (domainStatus apiKey) $ domain 
    myResponse <- lift $ apiResponse (opts apiKey) (unpack . constructUrl $ domain)
    status <- return (getStatus myResponse)
    -- myResponse is an ActionCtxT ctx m ByteString
    -- _ <- liftIO . C8.putStrLn $ myResponse L.^. W.responseBody
    renderStatus template domain status
handleRootWithXCode (Just "504") _apiKey template = do
  _ <- setStatus gatewayTimeout504
  maybeHost <- header "Host"
  let domain = Domain $ fromMaybe "" maybeHost in do
    -- domain is not really used?
    renderStatus template domain ReleaseUnhealthy
handleRootWithXCode (Just _) apiKey template = handleRootWithXCode Nothing apiKey template

renderStatus :: MonadIO m => Template -> Domain -> Types.Status -> ActionCtxT ctx (ReaderT (W.Options -> String -> IO (Response BL.ByteString)) m) a
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


