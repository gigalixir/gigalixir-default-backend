{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Spock
import Web.Spock.Config

import Control.Monad.IO.Class
import Data.String
import Data.Maybe

-- The import of ‘Control.Monad.Trans’ is redundant
-- import Control.Monad.Trans
import Data.IORef
-- The import of ‘Data.Monoid’ is redundant
-- import Data.Monoid
-- import qualified Data.Text as T
import Api
import Types

data MySession = EmptySession
data MyAppState = DummyAppState (IORef Int)

main :: IO ()
main =
    do ref <- newIORef 0
       spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (DummyAppState ref)
       runSpock 8080 (spock spockCfg app)

app :: SpockM () MySession MyAppState ()
app =
    do get root $ 
           handleRoot
       get ("healthz") $ text "ok"

handleRoot :: Control.Monad.IO.Class.MonadIO m => ActionCtxT ctx m a
handleRoot = do
  maybeHost <- header "Host"
  status <- liftIO $ domainStatus (Domain $ fromMaybe "" maybeHost)
  text $ fromString $ show status
