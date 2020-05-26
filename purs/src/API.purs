module API where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Either (Either)
import Effect.Aff (Aff)
import Affjax as AX
import Affjax (Response)
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

processResponse :: forall a. J.DecodeJson a => Aff (Either AX.Error (Response Json)) -> Aff (Either String a)
processResponse axResponse =
  runExceptT do
    response <- convertAffError axResponse
    decodeJson response.body

getMonads :: Aff (Either String (Array Monad))
getMonads = AX.get ResponseFormat.json "http://localhost:8090/monads" # processResponse

createMonad :: { name :: String, description :: String, rating :: Int } -> Aff (Either String Monad)
createMonad monadCreationData =
  AX.post ResponseFormat.json "http://localhost:8090/monads"
    ( Just $ RequestBody.json $ encodeJson monadCreationData
    )
    # processResponse

deleteMonad :: Int -> Aff (Either String Monad)
deleteMonad monadID =
  AX.delete ResponseFormat.json ("http://localhost:8090/monads/" <> show monadID)
    # processResponse
