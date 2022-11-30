module TimelineChart where

import Prelude

import React.Basic (ReactComponent)
import React.Basic as React
import React.Basic.Hooks (Component)

make :: Component { data :: Array { year :: Int, month :: String, value :: Number }, width :: Number }
make = pure (React.element _timelineChart)

foreign import _timelineChart
  :: ReactComponent
       { data ::
           Array
             { year :: Int
             , month :: String
             , value :: Number
             }
       , width :: Number
       }