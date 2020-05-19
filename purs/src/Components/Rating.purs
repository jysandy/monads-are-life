module Components.Rating where

import Prelude
import React.Basic (JSX, createComponent, makeStateless)
import React.Basic.DOM as R
import Data.List.Lazy as List

star :: JSX
star = R.span { className: "fas fa-star star" }

ratingStars :: Int -> JSX
ratingStars =
  makeStateless (createComponent "RatingStars")
    $ \n -> List.replicate n star # List.toUnfoldable # R.span_
