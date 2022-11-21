module App (mkApp) where

import Prelude

import Data.Array ((!!))
import Data.Array as Array
import Data.Date (Month(..), Year)
import Data.Either (Either(..))
import Data.Either as Either
import Data.Enum as Enum
import Data.Foldable as Foldable
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Traversable as Traversable
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\))
import Format.Int as Format
import React.Basic.DOM as DOM
import React.Basic.DOM.Events as DOM.Events
import React.Basic.Events as Events
import React.Basic.Hooks (Component, (/\))
import React.Basic.Hooks as Hooks
import Slider (mkRangeSlider)
import Temp.Data as Temp.Data

-- Note: needs to be a Tuple so that it's ordered by year _then_ month
type YearMonth = Year /\ Month

displayYearMonth :: YearMonth -> String
displayYearMonth (year /\ month) = show month <> " " <> show (Enum.fromEnum year)

type InflationData =
  { minKey :: YearMonth
  , maxKey :: YearMonth
  , allData :: Map YearMonth Number
  }

data InvariantError
  = BadYear Int
  | BadMonth String
  | EmptyMap

printInvariantError :: InvariantError -> String
printInvariantError = case _ of
  BadYear n -> "Received bad year: " <> show n
  BadMonth m -> "Received bad month: " <> m
  EmptyMap -> "Empty map"

appData :: Either InvariantError InflationData
appData = do
  dataArray <-
    Traversable.for Temp.Data.allData \datum -> do
      year :: Year <- Either.note (BadYear datum.year) do
        Enum.toEnum datum.year
      month <- Either.note (BadMonth datum.month) case datum.month of
        "January" -> Just January
        "February" -> Just February
        "March" -> Just March
        "April" -> Just April
        "May" -> Just May
        "June" -> Just June
        "July" -> Just July
        "August" -> Just August
        "September" -> Just September
        "October" -> Just October
        "November" -> Just November
        "December" -> Just December
        _ -> Nothing
      pure (Tuple (year /\ month) datum.value)
  let dataMap = Map.fromFoldable dataArray
  { minKey, maxKey } <- Either.note EmptyMap do
    min <- Map.findMin dataMap
    max <- Map.findMax dataMap
    pure { minKey: min.key, maxKey: max.key }
  pure { minKey, maxKey, allData: dataMap }

mkApp :: Component Unit
mkApp = do
  appContents <- mkAppContents
  Hooks.component "App" \_ -> Hooks.do
    pure case appData of
      Left error -> DOM.pre_ [ DOM.text (printInvariantError error) ]
      Right inflationData -> appContents inflationData

mkAppContents :: Component InflationData
mkAppContents = do
  rangeSlider <- mkRangeSlider
  Hooks.component "AppContents" \props -> Hooks.do
    let
      keys = Array.fromFoldable (Map.keys props.allData)
      minValue = 0
      maxValue = Array.length keys - 1
    thumbs /\ setThumbs <- Hooks.useState' { minThumb: minValue, maxThumb: maxValue }
    input /\ setInput <- Hooks.useState' ""

    let
      minKey = keys !! thumbs.minThumb
      maxKey = keys !! thumbs.maxThumb
      minV = minKey >>= \key -> Map.lookup key props.allData
      maxV = maxKey >>= \key -> Map.lookup key props.allData

    pure do
      DOM.div_
        [ DOM.h1_ [ DOM.text "Inflation data" ]
        , rangeSlider
            { minValue
            , maxValue
            , value: thumbs
            , onChange: setThumbs
            }
        , minKey
            # Foldable.foldMap \yearMonth -> DOM.p_ [ DOM.text (displayYearMonth yearMonth) ]
        , minV
            # Foldable.foldMap \value -> DOM.p_ [ DOM.text (show value) ]
        , maxKey
            # Foldable.foldMap \yearMonth -> DOM.p_ [ DOM.text (displayYearMonth yearMonth) ]
        , maxV
            # Foldable.foldMap \value -> DOM.p_ [ DOM.text (show value) ]
        , DOM.span
            { className: "dollar-input-group"
            , children:
                [ DOM.text "$"
                , DOM.input
                    { value: input
                    , onChange: Events.handler DOM.Events.targetValue do
                        Traversable.traverse_ \value ->
                          setInput (Format.formatString value)
                    , onBlur: Events.handler DOM.Events.targetValue do
                        (_ >>= Format.unformat)
                          >>> Traversable.traverse_ \value ->
                            setInput (Format.format value)
                    , type: "text"
                    , className: "dollar"
                    }
                ]
            }
        , DOM.p_
            [ DOM.text do
                case Format.unformat input of
                  Just n -> Maybe.fromMaybe "Oops!" do
                    x <- minV
                    y <- maxV
                    pure ("$" <> Format.format (Int.floor ((y / x) * Int.toNumber n)))
                  Nothing -> "Failed to parse " <> input <> " as a number"
            ]
        ]
