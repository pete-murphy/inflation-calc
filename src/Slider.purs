module Slider
  ( make
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
  , earlierLastSet :: Boolean
  }

foreign import _rangeSlider :: ReactComponent Props_

type Props =
  { minValue :: Int
  , maxValue :: Int
  , value :: Thumbs
  , onChange :: Thumbs -> Effect Unit
  , earlierLastSet :: Boolean
  }

make :: Component Props
make =
  Hooks.component "RangeSlider" \props -> Hooks.do
    pure
      ( Hooks.element
          _rangeSlider
          { step: 1
          , minValue: props.minValue
          , maxValue: props.maxValue
          , value: props.value
          , onChange: Uncurried.mkEffectFn1 props.onChange
          , earlierLastSet: props.earlierLastSet
          }
      )