module Components.MonadDisplay where

import Prelude
import React.Basic (Component, JSX, createComponent, makeStateless)
import React.Basic.DOM as R
import Components.Layout as L

type MonadFields r
  = { name :: String
    , description :: String
    , rating :: Int
    | r
    }

component :: forall r. Component (MonadFields r)
component = createComponent "MonadDisplay"

monadDisplay :: forall r. (MonadFields r) -> JSX
monadDisplay =
  makeStateless component
    $ \props ->
        L.row
          [ L.column 1 3 [ R.p_ [ R.text $ "Name: " <> props.name ] ]
          , L.column 1 3 [ R.p_ [ R.text $ "Description: " <> props.description ] ]
          , L.column 1 3 [ R.p_ [ R.text $ "Rating: " <> show props.rating ] ]
          ]
