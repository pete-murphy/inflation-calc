module LargeChart (make) where

import Prelude

import Data.Date (Date)
import Data.DateTime (DateTime(..))
import Data.JSDate (JSDate)
import Data.JSDate as JSDate
import React.Basic (ReactComponent)
import React.Basic as React
import React.Basic.Hooks (Component)

make
  :: Component
       { data :: Array { year :: Int, month :: String, value :: Number }
       , width :: Number
       , height :: Number
       , min :: Date
       , max :: Date
       }
make = pure
  ( React.element _largeChart <<< \props ->
      props
        { min = JSDate.fromDateTime (DateTime props.min bottom)
        , max = JSDate.fromDateTime (DateTime props.max bottom)
        }
  )

foreign import _largeChart
  :: ReactComponent
       { data ::
           Array
             { year :: Int
             , month :: String
             , value :: Number
             }
       , width :: Number
       , height :: Number
       , min :: JSDate
       , max :: JSDate
       }