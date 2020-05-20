module Components.App where

import Prelude
import Data.Either (Either(..))
import Effect.Aff (launchAff)
import React.Basic (Component, JSX, createComponent, make)
import React.Basic.DOM as R
import Effect.Console (log)
import Effect.Class (liftEffect)
import API as API
import Components.PureCSS as P
import Components.Rating (ratingStars)
import Data.Traversable (sequence)

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
    P.pageContainer
      [ R.h1_ [ R.text "M O N A D S" ]
      , P.column
          { childAlign: P.Left
          , width: P.Fraction 1 2
          , children:
              [ P.table
                  { header: map R.text [ "Name", "Description", "Rating" ]
                  , rows: (map monadToArray self.state)
                  }
              , R.p_ [ R.text "+" ]
              ]
          }
      ]
    where
    monadToArray = (sequence [ R.text <<< _.name, R.text <<< _.description, ratingStars <<< _.rating ])
