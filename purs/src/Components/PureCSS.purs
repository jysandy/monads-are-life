module Components.PureCSS (row, column, pageContainer, table, text, HorizontalAlignment(..), Fraction(..)) where

-- Wrappers over PureCSS's classes.
import Prelude
import React.Basic (JSX, createComponent, makeStateless)
import React.Basic.DOM as R
import Data.Array (singleton)

row :: Array JSX -> JSX
row =
  makeStateless (createComponent "Row")
    $ \children -> R.div { className: "pure-g row", children: children }

pageContainer :: Array JSX -> JSX
pageContainer =
  makeStateless (createComponent "PageContainer")
    $ \children ->
        R.div
          { className: "pure-g row"
          , children:
              [ column
                  { width: Fraction 1 5
                  , childAlign: Center
                  , children: []
                  }
              , column
                  { width: Fraction 3 5
                  , childAlign: Center
                  , children: children
                  }
              , column
                  { width: Fraction 1 5
                  , childAlign: Center
                  , children: []
                  }
              ]
          }

data HorizontalAlignment
  = Left
  | Right
  | Center

-- Fraction 1 3 => 1/3
data Fraction
  = Fraction Int Int

type ColumnProps
  = { childAlign :: HorizontalAlignment
    -- A column with width 1/3 => a column that takes up 1/3 of the width
    , width :: Fraction
    , children :: Array JSX
    }

columnClassName :: Fraction -> HorizontalAlignment -> String
columnClassName (Fraction numerator denominator) alignment =
  "pure-u-" <> show numerator <> "-" <> show denominator
    <> " column "
    <> alignClass alignment
  where
  alignClass Left = "column-left"

  alignClass Right = "column-right"

  alignClass Center = "column-center"

column :: ColumnProps -> JSX
column =
  makeStateless (createComponent "Column")
    $ \{ childAlign, width, children } ->
        R.div
          { className: columnClassName width childAlign
          , children: children
          }

text :: forall s. Show s => s -> Array JSX
text s = [ R.text (show s) ]

table :: forall r. { header :: Array JSX, rows :: Array (Array JSX) | r } -> JSX
table =
  makeStateless (createComponent "Table")
    $ \{ header, rows } ->
        R.table
          { className: "pure-table pure-table-horizontal"
          , children:
              [ R.thead_ [ R.tr_ $ map (R.th_ <<< singleton) header ]
              , R.tbody_ (map makeTR rows)
              ]
          }
  where
  makeTR row' = R.tr_ $ map (R.td_ <<< singleton) row'
