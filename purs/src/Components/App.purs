module Components.App where

import Prelude
import Data.Either (Either(..))
import Effect.Aff (Aff, launchAff)
import React.Basic.DOM as R
import Effect.Console (log)
import Effect.Class (liftEffect)
import React.Basic.DOM.Events (capture_)
import API as API
import Components.PureCSS as P
import Components.Rating (ratingStars)
import Components.CreateMonadForm (mkCreateMonadForm)
import Data.Traversable (sequence)
import React.Basic.Hooks as React
import React.Basic.Hooks (Component, component, useEffectOnce, useState, (/\))
import Effect (Effect)
import Data.Array (filter)

makeAPICall :: forall a. Aff (Either String a) -> (a -> Effect Unit) -> Effect Unit
makeAPICall aff onSuccess =
  void
    $ launchAff do
        result <- aff
        case result of
          Left error -> liftEffect $ log error
          Right a -> liftEffect $ onSuccess a

mkApp :: Component Unit
mkApp = do
  createMonadForm <- mkCreateMonadForm
  component "MonadsApp" \_ -> React.do
    state /\ setState <- useState []
    let
      resetState a = setState \_ -> a
    useEffectOnce do
      _ <- makeAPICall API.getMonads \monads -> resetState monads
      pure mempty
    let
      removeMonadWithID monadID = setState $ filter (\{ id } -> id /= monadID)

      addMonad monad = setState (\s -> append s [ monad ])

      deleteButton monadID =
        P.button
          { children: [ P.icon "trash" ]
          , onClick: capture_ (makeAPICall (API.deleteMonad monadID) \_ -> removeMonadWithID monadID)
          }
    pure
      $ P.pageContainer
          [ R.h1_ [ R.text "M O N A D S" ]
          , P.column
              { childAlign: P.Left
              , width: P.Fraction 1 2
              , children:
                  [ P.table
                      { header: map R.text [ "Name", "Description", "Rating", "" ]
                      , rows:
                          map
                            ( sequence
                                [ R.text <<< _.name
                                , R.text <<< _.description
                                , ratingStars <<< _.rating
                                , deleteButton <<< _.id
                                ]
                            )
                            state
                      }
                  , createMonadForm { onSuccess: addMonad }
                  ]
              }
          ]
