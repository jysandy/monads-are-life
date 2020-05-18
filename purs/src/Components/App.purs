module Components.App where

import Prelude
import Data.Either (Either(..))
import Components.MonadDisplay (monadDisplay)
import Effect.Aff (launchAff)
import React.Basic (Component, JSX, createComponent, make)
import React.Basic.DOM as R
import Effect.Console (log)
import Effect.Class (liftEffect)
import API as API

component :: Component Unit
component = createComponent "App"

app :: JSX
app = unit # make component { initialState, didMount, render }
  where
  initialState = []

  didMount self =
    void
      $ launchAff do
          result <- API.getMonads
          case result of
            Left error -> liftEffect $ log error
            Right monads -> liftEffect $ self.setState $ \_ -> monads

  render self =
    R.div_
      ( [ R.h1_ [ R.text "MONADS" ] ]
          <> (map monadDisplay self.state)
      )
