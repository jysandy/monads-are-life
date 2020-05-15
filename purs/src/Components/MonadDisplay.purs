module Components.MonadDisplay where

import Prelude
import React.Basic (Component, JSX, createComponent, makeStateless)
import React.Basic.DOM as R

type MonadFields
  = { name :: String
    , description :: String
    , rating :: Int
    }

type Props
  = { monad :: MonadFields
    }

component :: Component Props
component = createComponent "MonadDisplay"

monadDisplay :: Props -> JSX
monadDisplay =
  makeStateless component
    $ \props ->
        R.div_
          [ R.p_ [ R.text $ "Name: " <> props.monad.name ]
          , R.p_ [ R.text $ "Description: " <> props.monad.description ]
          , R.p_ [ R.text $ "Rating: " <> show props.monad.rating ]
          ]
