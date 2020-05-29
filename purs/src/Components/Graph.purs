module Components.Graph where

import Prelude
import React.Basic.Hooks as React
import React.Basic.DOM as R
import React.Basic.Hooks (Component, component, readRefMaybe, useEffect, useRef)
import Web.DOM.Node (Node)
import Effect (Effect)
import Data.Maybe (Maybe(..))
import Data.Nullable (null)

foreign import data Chart :: Type

foreign import makeChart :: Node -> Effect Chart

foreign import destroyChart :: Chart -> Effect Unit

mkGraph :: Component Unit
mkGraph = do
  component "Graph" \_ -> React.do
    canvasRef <- useRef null
    useEffect unit do
      maybeCanvasElement <- readRefMaybe canvasRef
      case maybeCanvasElement of
        Nothing -> pure mempty
        Just element -> do
          chart <- makeChart element
          pure $ destroyChart chart
    pure
      $ R.div
          { className: "chart-container"
          , children: [ R.canvas { ref: canvasRef } ]
          }
