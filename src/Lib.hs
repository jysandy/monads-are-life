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
import Control.Monad.Reader
import qualified Config
import GHC.Natural(naturalToInt)

type API =   "ping" :> Get '[JSON] String
      :<|> "monads" :> Get '[JSON] [Monad.Monad]
      :<|> "monads" :> ReqBody '[JSON] MonadCreateRequest :> Post '[JSON] Monad.Monad
      :<|> "monads" :> Capture "monadID" Integer :> Get '[JSON] Monad.Monad
      :<|> "monads" :> Capture "monadID" Integer :> Delete '[JSON] Monad.Monad

type MyHandler = ReaderT Config.Config Handler

server :: ServerT API MyHandler
server = return "pong"
    :<|> getMonads
    :<|> createMonad
    :<|> getMonadByID
    :<|> deleteMonad

getMonads :: MyHandler [Monad.Monad]
getMonads = liftIO Monad.fetchAll

createMonad :: MonadCreateRequest -> MyHandler Monad.Monad
createMonad (MonadCreateRequest name desc rating) = liftIO $ Monad.create name desc rating

getMonadByID :: Integer -> MyHandler Monad.Monad
getMonadByID monadID = do
  maybeMonad <- liftIO $ Monad.fetchByID monadID
  case maybeMonad of
    Just monad -> return monad
    Nothing -> throwError err404 { errBody = "That monad doesn't exist!" }

deleteMonad :: Integer -> MyHandler Monad.Monad
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

myHandlerToHandler :: Config.Config -> MyHandler a -> Handler a
myHandlerToHandler config myHandler = runReaderT myHandler config

app :: Config.Config -> Application
app config = serve api (hoistServer api (myHandlerToHandler config) server)

startApp :: IO ()
startApp = do
  config <- Config.read
  putStrLn "Starting server"
  run (naturalToInt $ Config.serverPort config) (app config)

api :: Proxy API
api = Proxy
