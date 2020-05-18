module API where

import Prelude
import Data.Either (Either(..))
import Effect.Aff (Aff)
import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.Except.Trans (ExceptT(..), except, runExceptT, withExceptT)
import Data.Argonaut.Decode (decodeJson)

type Monad
  = { id :: Int
    , name :: String
    , description :: String
    , rating :: Int
    }

getMonads :: Aff (Either String (Array Monad))
getMonads =
  runExceptT do
    response <-
      AX.get ResponseFormat.json "http://localhost:8090/monads"
        # ExceptT
        # withExceptT ((append "Affjax Error - ") <<< AX.printError)
    decodeJson response.body # except # withExceptT (append "Error decoding JSON - ")
