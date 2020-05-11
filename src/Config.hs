{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Config
  ( Config(..)
  , read
  )
where

import           Prelude                 hiding ( read )
import           Dhall
import           GHC.Generics

data Config = Config {serverPort :: Natural, dbName :: String} deriving (Generic, Show)
instance FromDhall Config

read :: IO Config
read = input auto "./config.dhall"
