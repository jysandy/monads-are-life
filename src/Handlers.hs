{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

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
import qualified Config
import           Data.Aeson
import           Servant                 hiding ( Handler )
import qualified Servant                        ( Handler )
import qualified Monad
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Text

type Handler = ReaderT Config.Config Servant.Handler

getMonads :: Handler [Monad.Monad]
getMonads = liftIO Monad.fetchAll

createMonad :: MonadCreateRequest -> Handler Monad.Monad
createMonad (MonadCreateRequest name desc rating) =
  liftIO $ Monad.create name desc rating

getMonadByID :: Integer -> Handler Monad.Monad
getMonadByID monadID = do
  maybeMonad <- liftIO $ Monad.fetchByID monadID
  case maybeMonad of
    Just monad -> return monad
    Nothing    -> throwError err404 { errBody = "That monad doesn't exist!" }

deleteMonad :: Integer -> Handler Monad.Monad
deleteMonad monadID = do
  maybeMonad <- liftIO $ Monad.delete monadID
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

toServantHandler :: Config.Config -> Handler a -> Servant.Handler a
toServantHandler config handler = runReaderT handler config
