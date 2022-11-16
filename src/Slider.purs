module Slider
  ( mkRangeSlider
  ) where

import Prelude

import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\))
import Effect (Effect)
import Effect.Uncurried (EffectFn1)
import Effect.Uncurried as Uncurried
import React.Basic.Hooks (Component, ReactComponent)
import React.Basic.Hooks as Hooks

type PropsF x =
  { value :: x
  , onChange :: EffectFn1 x Unit
  , fromForeign ::
      Int
      -> Int
      -> x
  , toForeign :: x -> Array Int
  }

type Props_ =
  { label :: String
  , minValue :: Int
  , maxValue :: Int
  , step :: Int
  , mkStateProps :: forall r. (forall x. PropsF x -> r) -> r
  }

foreign import rangeSlider_ :: ReactComponent Props_

type Props =
  { minValue :: Int
  , maxValue :: Int
  , value :: Int /\ Int
  , onChange :: Int /\ Int -> Effect Unit
  }

mkRangeSlider :: Component Props
mkRangeSlider =
  Hooks.component "RangeSlider" \props -> Hooks.do
    let
      mkStateProps :: forall r. (forall x. PropsF x -> r) -> r
      mkStateProps run = run
        { fromForeign: Tuple
        , toForeign: \(Tuple n m) -> [ n, m ]
        , onChange: Uncurried.mkEffectFn1 props.onChange
        , value: props.value
        }
    pure
      ( Hooks.element
          rangeSlider_
          { label: "Years"
          , step: 1
          , minValue: props.minValue
          , maxValue: props.maxValue
          , mkStateProps
          }
      )