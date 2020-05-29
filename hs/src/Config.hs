{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Config
  ( Config(..)
  , read
  , getConfig
  ) where

import           Control.Monad.Reader
import           Dhall
import           GHC.Generics
import           Prelude              hiding (read)

data Config =
  Config
    { serverPort :: Natural
    , dbName     :: String
    }
  deriving (Generic, Show)

instance FromDhall Config

read :: IO Config
read = input auto "./config.dhall"

getConfig :: Monad m => ReaderT Config m Config
getConfig = ask
