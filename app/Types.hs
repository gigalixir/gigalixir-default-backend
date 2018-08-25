module Types where

newtype Domain = Domain String
                 deriving (Show)

data Status = Error
            | DomainNotFound
            | AppNotFound
            | ReleaseNotFound
            | ReplicasNotFound
            | Ok
            deriving (Show)

