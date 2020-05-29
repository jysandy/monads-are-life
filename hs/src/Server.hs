{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Server(startServer) where

import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors
import           Network.HTTP.Types.Method
import           Servant
import qualified Config
import qualified Handlers
import           GHC.Natural(naturalToInt)
import qualified Monad

type API =   "ping" :> Get '[JSON] String
      :<|> "monads" :> Get '[JSON] [Monad.Monad]
      :<|> "monads" :> ReqBody '[JSON] Handlers.MonadCreateRequest :> Post '[JSON] Monad.Monad
      :<|> "monads" :> Capture "monadID" Integer :> Get '[JSON] Monad.Monad
      :<|> "monads" :> Capture "monadID" Integer :> Delete '[JSON] Monad.Monad

server :: ServerT API Handlers.Handler
server = return "pong"
    :<|> Handlers.getMonads
    :<|> Handlers.createMonad
    :<|> Handlers.getMonadByID
    :<|> Handlers.deleteMonad

app :: Config.Config -> Application
app config = let corsMiddleware = cors $ \_ -> Just $ CorsResourcePolicy { corsOrigins = Nothing
                                                                         , corsMethods = [methodGet, methodPost, methodHead, methodPut, methodDelete]
                                                                         , corsRequestHeaders = simpleHeaders
                                                                         , corsExposedHeaders = Just simpleResponseHeaders
                                                                         , corsMaxAge = Nothing
                                                                         , corsVaryOrigin = False
                                                                         , corsRequireOrigin = False
                                                                         , corsIgnoreFailures = False}
    in corsMiddleware $ serve api (hoistServer api (Handlers.toServantHandler config) server)

startServer :: Config.Config -> IO ()
startServer config = run (naturalToInt $ Config.serverPort config) (app config)
  
api :: Proxy API
api = Proxy
