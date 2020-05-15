{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Handlers
  ( getMonads
  , createMonad
  , getMonadByID
  , deleteMonad
  , MonadCreateRequest
  , toServantHandler
  , Handler
  )
where

import           Control.Monad.Trans
import           GHC.Generics
import           Config                         ( getConfig
                                                , Config(..)
                                                )
import           Data.Aeson
import           Servant                 hiding ( Handler )
import qualified Servant                        ( Handler )
import qualified Monad
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Text

type Handler = ReaderT Config Servant.Handler

getMonads :: Handler [Monad.Monad]
getMonads = do
  Config { dbName } <- getConfig
  liftIO $ Monad.fetchAll dbName

createMonad :: MonadCreateRequest -> Handler Monad.Monad
createMonad (MonadCreateRequest name desc rating) = do
  Config { dbName } <- getConfig
  liftIO $ Monad.create dbName name desc rating

getMonadByID :: Integer -> Handler Monad.Monad
getMonadByID monadID = do
  Config { dbName } <- getConfig
  maybeMonad        <- liftIO $ Monad.fetchByID dbName monadID
  case maybeMonad of
    Just monad -> return monad
    Nothing    -> throwError err404 { errBody = "That monad doesn't exist!" }

deleteMonad :: Integer -> Handler Monad.Monad
deleteMonad monadID = do
  Config { dbName } <- getConfig
  maybeMonad        <- liftIO $ Monad.delete dbName monadID
  case maybeMonad of
    Just monad -> return monad
    Nothing    -> throwError err404 { errBody = "That monad doesn't exist!" }

data MonadCreateRequest = MonadCreateRequest
                          { name :: Text
                          , description :: Text
                          , rating :: Int
                          } deriving Generic

instance FromJSON MonadCreateRequest
instance ToJSON MonadCreateRequest

toServantHandler :: Config -> Handler a -> Servant.Handler a
toServantHandler config handler = runReaderT handler config
