module Types where

import Data.Text

newtype Domain = Domain Text
                 deriving (Show)

data Status = Error
            | DomainNotFound
            | AppNotFound
            | ReleaseNotFound
            | ReplicasNotFound
            | Ok
            deriving (Show)

type ApiKey = Text
