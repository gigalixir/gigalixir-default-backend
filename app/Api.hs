{-# LANGUAGE OverloadedStrings #-}
module Api where

import Data.ByteString.Lazy.Internal
import Data.Text
import Types
import Network.Wreq -- (getWith, Response, Options, defaults, basicAuth, auth)
import Control.Lens
import Data.Aeson.Lens (_String, key)

domainStatus :: Domain -> IO Types.Status
domainStatus domain = do
  print $ constructUrl domain
  response <- getWith opts (constructUrl domain)
  return (constructStatus response)

constructUrl :: Domain -> String
constructUrl (Domain domain) = "https://api.gigalixir.com/api/default_backend/domain?domain=" <> domain

constructStatus :: Response Data.ByteString.Lazy.Internal.ByteString -> Types.Status
constructStatus r = 
  convertStatus $ r ^. responseBody . key "data" . key "status" . _String

convertStatus :: Text -> Types.Status
convertStatus "domain_not_found" = DomainNotFound
convertStatus "app_not_found" = AppNotFound
convertStatus "release_not_found" = ReleaseNotFound
convertStatus "replicas_not_found" = ReplicasNotFound
convertStatus "ok" = Ok
convertStatus _ = Error

opts :: Options
opts = defaults & auth ?~ basicAuth "" "e0344fac-3597-43d6-a0c0-4acd628aa614"
