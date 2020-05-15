module Components.MonadDisplay where

import Prelude
import React.Basic (Component, JSX, createComponent, makeStateless)
import React.Basic.DOM as R

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
        R.div_
          [ R.p_ [ R.text $ "Name: " <> props.name ]
          , R.p_ [ R.text $ "Description: " <> props.description ]
          , R.p_ [ R.text $ "Rating: " <> show props.rating ]
          ]
