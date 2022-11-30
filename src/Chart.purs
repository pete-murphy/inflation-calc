module Chart where

import Prelude

import React.Basic (ReactComponent)
import React.Basic as React
import React.Basic.Hooks (Component)

make :: Component (Array { year :: Int, month :: String, value :: Number })
make = pure (React.element _chart <<< \data' -> { data: data' })

foreign import _chart
  :: ReactComponent
       { data ::
           Array
             { year :: Int
             , month :: String
             , value :: Number
             }
       }