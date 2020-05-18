module Components.Layout (row, column) where

import Prelude
import React.Basic (JSX, createComponent, makeStateless)
import React.Basic.DOM as R

row :: Array JSX -> JSX
row =
  makeStateless (createComponent "Row")
    $ \children -> R.div { className: "pure-g", children: children }

unitClassName :: Int -> Int -> String
unitClassName numerator denominator = "pure-u-" <> show numerator <> "-" <> show denominator

-- column 1 3 => a column that takes up 1/3 of the width.
column :: Int -> Int -> Array JSX -> JSX
column numerator denominator children =
  R.div
    { className: unitClassName numerator denominator
    , children: children
    }
