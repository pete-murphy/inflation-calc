module Slider
  ( mkRangeSlider
  ) where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn1)
import Effect.Uncurried as Uncurried
import React.Basic.Hooks (Component, ReactComponent)
import React.Basic.Hooks as Hooks

type Thumbs =
  { minThumb :: Int
  , maxThumb :: Int
  }

type Props_ =
  { minValue :: Int
  , maxValue :: Int
  , step :: Int
  , value :: Thumbs
  , onChange :: EffectFn1 Thumbs Unit
  }

foreign import rangeSlider_ :: ReactComponent Props_

type Props =
  { minValue :: Int
  , maxValue :: Int
  , value :: Thumbs
  , onChange :: Thumbs -> Effect Unit
  }

mkRangeSlider :: Component Props
mkRangeSlider =
  Hooks.component "RangeSlider" \props -> Hooks.do
    pure
      ( Hooks.element
          rangeSlider_
          { step: 1
          , minValue: props.minValue
          , maxValue: props.maxValue
          , value: props.value
          , onChange: Uncurried.mkEffectFn1 props.onChange
          }
      )