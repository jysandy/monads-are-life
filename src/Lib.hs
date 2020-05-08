{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( startApp
    , app
    ) where

import           Data.Aeson
import           Network.Wai.Handler.Warp
import           Servant
import qualified Monad
import           Control.Monad.Trans
import           GHC.Generics
import           Data.Text
import           Control.Monad.Except

type API =   "ping" :> Get '[JSON] String
      :<|> "monads" :> Get '[JSON] [Monad.Monad]
      :<|> "monads" :> ReqBody '[JSON] MonadCreateRequest :> Post '[JSON] Monad.Monad
      :<|> "monads" :> Capture "monadID" Integer :> Get '[JSON] Monad.Monad
      :<|> "monads" :> Capture "monadID" Integer :> Delete '[JSON] Monad.Monad

server :: Server API
server = return "pong"
    :<|> getMonads
    :<|> createMonad
    :<|> getMonadByID
    :<|> deleteMonad

getMonads :: Handler [Monad.Monad]
getMonads = liftIO Monad.fetchAll

createMonad :: MonadCreateRequest -> Handler Monad.Monad
createMonad (MonadCreateRequest name desc rating) = liftIO $ Monad.create name desc rating

getMonadByID :: Integer -> Handler Monad.Monad
getMonadByID monadID = do
  maybeMonad <- liftIO $ Monad.fetchByID monadID
  case maybeMonad of
    Just monad -> return monad
    Nothing -> throwError err404 { errBody = "That monad doesn't exist!" }

deleteMonad :: Integer -> Handler Monad.Monad
deleteMonad monadID = do
  maybeMonad <- liftIO $ Monad.delete monadID
  case maybeMonad of
    Just monad -> return monad
    Nothing -> throwError err404 { errBody = "That monad doesn't exist!" }

data MonadCreateRequest = MonadCreateRequest 
                          { name :: Text
                          , description :: Text
                          , rating :: Int
                          } deriving Generic

instance FromJSON MonadCreateRequest
instance ToJSON MonadCreateRequest

app :: Application
app = serve api server

startApp :: IO ()
startApp = do
  putStrLn "Starting server"
  run 8090 app

api :: Proxy API
api = Proxy
