module Components.CreateMonadForm (createMonadForm) where

import Prelude
import Data.Either (Either(..))
import Data.Int as I
import Effect.Aff (launchAff)
import React.Basic (JSX, createComponent, make)
import React.Basic.DOM as R
import React.Basic.DOM.Events (capture_, capture, targetValue)
import Effect.Console (log)
import Effect.Class (liftEffect)
import Effect (Effect)
import API as API
import Components.PureCSS as P
import Data.Maybe (fromMaybe)

type Props
  = { onSuccess :: API.Monad -> Effect Unit }

createMonadForm :: Props -> JSX
createMonadForm = make (createComponent "CreateMonadForm") { initialState, render }
  where
  initialState = { open: false, name: "", description: "", rating: 3 }

  render self =
    if self.state.open then
      P.column
        { children:
            [ R.label_
                [ R.text "Name: "
                , R.input
                    { value: self.state.name
                    , onChange: capture targetValue updateName
                    }
                ]
            , R.label_
                [ R.text "Description: "
                , R.input
                    { value: self.state.description
                    , onChange: capture targetValue updateDescription
                    }
                ]
            , R.label_
                [ R.text "Rating: "
                , R.input
                    { value: show self.state.rating
                    , type: "number"
                    , min: "0"
                    , max: "5"
                    , onChange:
                        capture targetValue updateRating
                    }
                ]
            , P.row
                [ P.column
                    { childAlign: P.Left
                    , width:
                        P.Fraction 1 4
                    , children:
                        [ P.button
                            { children: [ R.text "Create" ]
                            , onClick: capture_ createMonad
                            }
                        ]
                    }
                , P.column
                    { childAlign: P.Left
                    , width:
                        P.Fraction 1 4
                    , children:
                        [ P.button
                            { children: [ R.text "Cancel" ]
                            , onClick: capture_ $ self.setState $ \s -> s { open = false }
                            }
                        ]
                    }
                ]
            ]
        , childAlign: P.Left
        , width: P.Fraction 1 2
        }
    else
      P.button
        { children: [ P.icon "plus" ]
        , onClick: capture_ $ self.setState $ \s -> s { open = true }
        }
    where
    createMonad =
      void
        $ launchAff do
            result <-
              API.createMonad
                { name: self.state.name
                , description: self.state.description
                , rating: self.state.rating
                }
            case result of
              Left error -> liftEffect $ log error
              Right monad -> liftEffect $ self.props.onSuccess monad

    updateName targetValue = self.setState $ \s -> s { name = fromMaybe "" targetValue }

    updateDescription targetValue = self.setState $ \s -> s { description = fromMaybe "" targetValue }

    updateRating targetValue =
      self.setState
        $ \s ->
            s
              { rating =
                fromMaybe 0 do
                  numberString <- targetValue
                  I.fromString numberString
              }
