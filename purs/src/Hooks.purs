module Hooks where

import Prelude
import Data.Tuple (Tuple)
import React.Basic.Hooks (Hook, UseState, useState, (/\))
import React.Basic.Events (EventHandler)
import React.Basic.Hooks as React
import React.Basic.DOM.Events (capture, targetValue)
import Data.Int as I
import Data.Maybe (fromMaybe)

useStringInput :: String -> Hook (UseState String) (Tuple String EventHandler)
useStringInput initialValue = React.do
  value /\ setValue <- useState initialValue
  pure
    $ ( value
          /\ ( capture targetValue \newValue ->
                ( setValue \_ ->
                    fromMaybe "" newValue
                )
            )
      )

useNumberInput :: Int -> Hook (UseState Int) (Tuple Int EventHandler)
useNumberInput initialValue = React.do
  value /\ setValue <- useState initialValue
  pure $ value
    /\ ( capture targetValue \newString ->
          ( setValue \_ ->
              fromMaybe 0 do
                numberString <- newString
                I.fromString numberString
          )
      )
