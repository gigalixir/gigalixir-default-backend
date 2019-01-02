{-# LANGUAGE OverloadedStrings #-}
module Spec where

import Main (app)
import Test.Hspec
import Test.Hspec.Wai
import Web.Spock (spockAsApp)
import qualified Network.Wreq as W
import qualified Data.ByteString.Lazy as BL
import Control.Monad.Trans.Reader
import Network.HTTP.Client.Internal
import Control.Monad.IO.Class (MonadIO, liftIO)
import Network.HTTP.Types.Status
import Network.HTTP.Types.Version (http11)

main :: IO ()
main = hspec spec


okResponse :: Response BL.ByteString
okResponse = Response
  { responseStatus = mkStatus 200 "success"
  , responseVersion = http11
  , responseHeaders = []
  , responseBody = "{\"data\":{\"status\":\"ok\"}}"
  , responseCookieJar = createCookieJar []
  , responseClose' = ResponseClose (return () :: IO ())
  }

mockRunner :: MonadIO m => ReaderT (W.Options -> String -> IO (Response BL.ByteString)) m a -> m a
mockRunner r = runReaderT r mockRequester
  where mockRequester _ _ = return okResponse

spec :: Spec
spec =
    with (spockAsApp $ app mockRunner) $
        do describe "GET /" $
               do it "serves the home page" $
                      get "/" `shouldRespondWith` "Sorry, looks like your app is running, but we're still setting up the load balancer. Try again in a few seconds.\n" {matchStatus = 404}

