module Components.CreateMonadForm (mkCreateMonadForm) where

import Prelude
import Data.Either (Either(..))
import Effect.Aff (launchAff)
import React.Basic.Hooks (Component, component, useState, (/\))
import React.Basic.DOM as R
import React.Basic.DOM.Events (capture_)
import Effect.Console (log)
import Effect.Class (liftEffect)
import Effect (Effect)
import API as API
import Components.PureCSS as P
import React.Basic.Hooks as React
import Hooks (useStringInput, useNumberInput)

type Props
  = { onSuccess :: API.Monad -> Effect Unit }

mkCreateMonadForm :: Component Props
mkCreateMonadForm = component "CreateMonadForm" render
  where
  render { onSuccess } = React.do
    open /\ setOpen <- useState false
    name /\ onNameChange <- useStringInput ""
    description /\ onDescriptionChange <- useStringInput ""
    rating /\ onRatingChange <- useNumberInput 3
    let
      closeForm = setOpen \_ -> false

      openForm = setOpen \_ -> true
    pure
      $ if open then
          P.column
            { children:
                [ R.label_
                    [ R.text "Name: "
                    , R.input
                        { value: name
                        , onChange: onNameChange
                        }
                    ]
                , R.label_
                    [ R.text "Description: "
                    , R.input
                        { value: description
                        , onChange: onDescriptionChange
                        }
                    ]
                , R.label_
                    [ R.text "Rating: "
                    , R.input
                        { value: show rating
                        , type: "number"
                        , min: "0"
                        , max: "5"
                        , onChange: onRatingChange
                        }
                    ]
                , P.row
                    [ P.column
                        { childAlign: P.Left
                        , width:
                            P.Fraction 3 24
                        , children:
                            [ P.button
                                { children: [ R.text "Create" ]
                                , onClick: capture_ (createMonad name description rating)
                                }
                            ]
                        }
                    , P.column
                        { childAlign: P.Left
                        , width:
                            P.Fraction 3 24
                        , children:
                            [ P.button
                                { children: [ R.text "Cancel" ]
                                , onClick: capture_ closeForm
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
            , onClick: capture_ openForm
            }
    where
    createMonad name description rating =
      void
        $ launchAff do
            result <-
              API.createMonad
                { name: name
                , description: description
                , rating: rating
                }
            case result of
              Left error -> liftEffect $ log error
              Right monad -> liftEffect $ onSuccess monad


