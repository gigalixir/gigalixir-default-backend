{-# LANGUAGE OverloadedStrings #-}
module Api where

import qualified Data.ByteString.Lazy as BL
import Data.Text
import Data.Text.Encoding
import Types
import Network.Wreq
import Control.Lens
import Data.Aeson.Lens (_String, key)
    
class Monad m => MonadHttp m where
  getWith :: Options -> String -> m (Response BL.ByteString)

instance MonadHttp IO where
  getWith = Network.Wreq.getWith

domainStatus :: MonadHttp m => ApiKey -> Domain -> m Types.Status
domainStatus apiKey domain = do
  response <- Api.getWith (opts apiKey) (unpack $ constructUrl domain)
  return (getStatus response)

-- TODO: make this a config value
constructUrl :: Domain -> Text
constructUrl (Domain domain) = "https://api.gigalixir.com/api/default_backend/domain?domain=" <> domain

getStatus :: Response BL.ByteString -> Types.Status
getStatus r = 
  convertStatus $ r ^. responseBody . key "data" . key "status" . _String

convertStatus :: Text -> Types.Status
convertStatus "domain_not_found" = DomainNotFound
convertStatus "app_not_found" = AppNotFound
convertStatus "release_not_found" = ReleaseNotFound
convertStatus "replicas_not_found" = ReplicasNotFound
convertStatus "ok" = Ok
convertStatus _ = Error

-- TODO: make this a config value
opts :: ApiKey -> Options
opts apiKey = defaults & auth ?~ basicAuth "" (encodeUtf8 apiKey)

