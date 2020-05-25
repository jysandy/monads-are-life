module Components.App where

import Prelude
import Data.Either (Either(..))
import Effect.Aff (launchAff)
--import React.Basic (Component, JSX, createComponent, make)
import React.Basic.DOM as R
import Effect.Console (log)
import Effect.Class (liftEffect)
import API as API
import Components.PureCSS as P
import Components.Rating (ratingStars)
import Components.CreateMonadForm (mkCreateMonadForm)
import Data.Traversable (sequence)
import React.Basic.Hooks as React
import React.Basic.Hooks (Component, component, useEffect, useState, (/\))

mkApp :: Component Unit
mkApp = do
  createMonadForm <- mkCreateMonadForm
  component "MonadsApp" \_ -> React.do
    state /\ setState <- useState []
    useEffect state do
      _ <-
        launchAff do
          result <- API.getMonads
          case result of
            Left error -> liftEffect $ log error
            Right monads -> liftEffect $ setState $ \_ -> monads
      pure mempty
    pure
      $ P.pageContainer
          [ R.h1_ [ R.text "M O N A D S" ]
          , P.column
              { childAlign: P.Left
              , width: P.Fraction 1 2
              , children:
                  [ P.table
                      { header: map R.text [ "Name", "Description", "Rating" ]
                      , rows: (map monadToArray state)
                      }
                  , createMonadForm { onSuccess: \monad -> setState (\s -> append s [ monad ]) }
                  ]
              }
          ]
  where
  monadToArray = (sequence [ R.text <<< _.name, R.text <<< _.description, ratingStars <<< _.rating ])
