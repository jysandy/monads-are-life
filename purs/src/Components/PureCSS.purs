module Components.PureCSS (row, column, pageContainer, table, text) where

-- Wrappers over PureCSS's classes.
import Prelude
import React.Basic (JSX, createComponent, makeStateless)
import React.Basic.DOM as R
import Data.Array (singleton)

row :: Array JSX -> JSX
row =
  makeStateless (createComponent "Row")
    $ \children -> R.div { className: "pure-g", children: children }

pageContainer :: Array JSX -> JSX
pageContainer =
  makeStateless (createComponent "PageContainer")
    $ \children ->
        R.div
          { className: "pure-g"
          , children:
              [ column 1 5 []
              , column 3 5 children
              , column 1 5 []
              ]
          }

columnClassName :: Int -> Int -> String
columnClassName numerator denominator =
  "pure-u-" <> show numerator <> "-" <> show denominator
    <> " column centred-container"

-- column 1 3 => a column that takes up 1/3 of the width.
column :: Int -> Int -> Array JSX -> JSX
column numerator denominator =
  makeStateless (createComponent $ "Column-" <> show numerator <> "-" <> show denominator)
    $ \children ->
        R.div
          { className: columnClassName numerator denominator
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
