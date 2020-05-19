module Components.PureCSS (row, column, pageContainer, table) where

-- Wrappers over PureCSS's classes.
import Prelude
import React.Basic (JSX, createComponent, makeStateless)
import React.Basic.DOM as R

row :: Array JSX -> JSX
row =
  makeStateless (createComponent "Row")
    $ \children -> R.div { className: "pure-g", children: children }

pageContainer :: Array JSX -> JSX
pageContainer =
  makeStateless (createComponent "PageContainer")
    $ \children ->
        row
          [ column 1 5 []
          , column 3 5 children
          , column 1 5 []
          ]

unitClassName :: Int -> Int -> String
unitClassName numerator denominator = "pure-u-" <> show numerator <> "-" <> show denominator

-- column 1 3 => a column that takes up 1/3 of the width.
column :: Int -> Int -> Array JSX -> JSX
column numerator denominator =
  makeStateless (createComponent $ "Column-" <> show numerator <> "-" <> show denominator)
    $ \children ->
        R.div
          { className: unitClassName numerator denominator
          , children: children
          }

text :: String -> Array JSX
text s = [ R.text s ]

table :: forall r. { header :: Array String, rows :: Array (Array String) | r } -> JSX
table =
  makeStateless (createComponent "Table")
    $ \{ header, rows } ->
        R.table
          { className: "pure-table pure-table-horizontal"
          , children:
              [ R.thead_ [ R.tr_ $ map (R.th_ <<< text) header ]
              , R.tbody_ (map makeTR rows)
              ]
          }
  where
  makeTR row' = R.tr_ $ map (R.td_ <<< text) row'
