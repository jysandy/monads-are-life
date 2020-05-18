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
        R.div
          { className: "pure-g"
          , children:
              [ R.div
                  { className: "pure-u-1-3"
                  , children: [ R.p_ [ R.text $ "Name: " <> props.name ] ]
                  }
              , R.div
                  { className: "pure-u-1-3"
                  , children: [ R.p_ [ R.text $ "Description: " <> props.description ] ]
                  }
              , R.div
                  { className: "pure-u-1-3"
                  , children: [ R.p_ [ R.text $ "Rating: " <> show props.rating ] ]
                  }
              ]
          }
