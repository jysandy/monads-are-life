module Components.App where

import Prelude
import Components.MonadDisplay (monadDisplay)
import React.Basic (Component, JSX, createComponent, makeStateless)
import React.Basic.DOM as R

component :: Component Unit
component = createComponent "App"

app :: JSX
app =
  unit
    # makeStateless component \_ ->
        R.div_
          [ R.h1_ [ R.text "MONADS" ]
          , monadDisplay { name: "Maybe", description: "Monad to work with nullables", rating: 4 } 
          , monadDisplay { name: "State", description: "Monad to emulate mutable state", rating: 3 } 
          ]
