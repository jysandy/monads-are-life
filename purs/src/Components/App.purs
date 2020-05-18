module Components.App where

import Prelude
import Data.Either (Either(..))
import Components.MonadDisplay (monadDisplay)
import Effect.Aff (launchAff)
import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import React.Basic (Component, JSX, createComponent, make)
import React.Basic.DOM as R
import Effect.Console (log)
import Effect.Class (liftEffect)
import Data.Argonaut.Decode (decodeJson)
import Control.Monad.Except.Trans (ExceptT(..), except, runExceptT, withExceptT)

component :: Component Unit
component = createComponent "App"

type Monad
  = { id :: Int
    , name :: String
    , description :: String
    , rating :: Int
    }

app :: JSX
app = unit # make component { initialState, didMount, render }
  where
  initialState = [] :: Array Monad

  didMount self =
    void
      $ launchAff do
          result <-
            runExceptT do
              response <- AX.get ResponseFormat.json "http://localhost:8090/monads" # ExceptT # withExceptT ((append "Affjax Error - ") <<< AX.printError)
              body <- decodeJson response.body # except # withExceptT (append "Error decoding JSON - ")
              liftEffect $ self.setState $ \_ -> body
          case result of
            Left error -> liftEffect $ log error
            Right r -> pure r

  render self =
    R.div_
      ( [ R.h1_ [ R.text "MONADS" ] ]
          <> (map monadDisplay self.state)
      )
