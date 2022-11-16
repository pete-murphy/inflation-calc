module Temp.Data where

foreign import allData
  :: Array
       { year :: Int
       , month :: String
       , value :: Number
       }
