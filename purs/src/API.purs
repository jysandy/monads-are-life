module API where

import Prelude
import Data.Maybe
import Data.Either (Either(..))
import Effect.Aff (Aff)
import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Affjax.RequestBody as RequestBody
import Control.Monad.Except.Trans (ExceptT(..), except, runExceptT, withExceptT)
import Data.Argonaut.Decode (decodeJson, class DecodeJson) as J
import Data.Argonaut.Encode (encodeJson)
import Data.Argonaut.Core (Json)

type Monad
  = { id :: Int
    , name :: String
    , description :: String
    , rating :: Int
    }

decodeJson :: forall a. J.DecodeJson a => Json -> ExceptT String Aff a
decodeJson jsonBody = J.decodeJson jsonBody # except # withExceptT (append "Error decoding JSON - ")

convertAffError :: forall a. Aff (Either AX.Error a) -> ExceptT String Aff a
convertAffError a = a # ExceptT # withExceptT ((append "Affjax Error - ") <<< AX.printError)

getMonads :: Aff (Either String (Array Monad))
getMonads =
  runExceptT do
    response <-
      AX.get ResponseFormat.json "http://localhost:8090/monads"
        # convertAffError
    decodeJson response.body

createMonad :: { name :: String, description :: String, rating :: Int } -> Aff (Either String Monad)
createMonad monadCreationData =
  runExceptT do
    response <-
      AX.post ResponseFormat.json "http://localhost:8090/monads"
        ( Just $ RequestBody.json $ encodeJson monadCreationData
        )
        # convertAffError
    decodeJson response.body
