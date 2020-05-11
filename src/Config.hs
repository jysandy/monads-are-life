{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Config
  ( Config(..)
  , read
  , getConfig
  )
where

import           Prelude                 hiding ( read )
import           Dhall
import           GHC.Generics
import           Control.Monad.Reader

data Config = Config {serverPort :: Natural, dbName :: String} deriving (Generic, Show)
instance FromDhall Config

read :: IO Config
read = input auto "./config.dhall"

getConfig :: Monad m => ReaderT Config m Config
getConfig = ask
