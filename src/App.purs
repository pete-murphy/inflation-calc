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
import Debug as Debug
import Effect (Effect)
import Format.Int as Format
import React.Basic.DOM as DOM
import React.Basic.DOM.Events as DOM.Events
import React.Basic.Events as Events
import React.Basic.Hooks (Component, Reducer, (/\))
import React.Basic.Hooks as Hooks
import Slider (mkRangeSlider)
import Temp.Data as Temp.Data

-- Note: needs to be a Tuple so that it's ordered by year _then_ month
type YearMonth = Year /\ Month

displayYearMonth :: YearMonth -> String
displayYearMonth (year /\ month) = show month <> " " <> show (Enum.fromEnum year)

type InflationData =
  { thumbs :: Thumbs
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
  thumbs <- Either.note EmptyMap do
    minThumb <- Map.findMin dataMap
    maxThumb <- Map.findMax dataMap
    pure { minThumb, maxThumb }
  pure { thumbs, allData: dataMap }

mkApp :: Component Unit
mkApp = do
  appContents <- mkAppContents
  Hooks.component "App" \_ -> Hooks.do
    pure case appData of
      Left error -> DOM.pre_ [ DOM.text (printInvariantError error) ]
      Right inflationData -> appContents inflationData

type Thumbs =
  { minThumb :: { key :: YearMonth, value :: Number }
  , maxThumb :: { key :: YearMonth, value :: Number }
  }

type State =
  { earlier :: String
  , later :: String
  , thumbs :: Thumbs
  -- TODO: Must be a better way
  , earlierLastSet :: Boolean
  }

data Action
  = Earlier String
  | Later String
  | SetThumbs Thumbs

mkReducer :: Effect (Reducer State Action)
mkReducer = Hooks.mkReducer \state -> case _ of
  Earlier earlier -> do
    let f n = Int.floor ((state.thumbs.maxThumb.value / state.thumbs.minThumb.value) * Int.toNumber n)
    { earlier: Format.formatString earlier
    , later: Maybe.fromMaybe state.later (earlier # Format.unformat >>> map (f >>> Format.format))
    , earlierLastSet: true
    , thumbs: state.thumbs
    }
  Later later -> do
    let f n = Int.floor ((state.thumbs.minThumb.value / state.thumbs.maxThumb.value) * Int.toNumber n)
    { earlier: Maybe.fromMaybe state.earlier (later # Format.unformat >>> map (f >>> Format.format))
    , later: Format.formatString later
    , earlierLastSet: false
    , thumbs: state.thumbs
    }
  SetThumbs thumbs -> do
    let
      d =
        if state.earlierLastSet then state.thumbs.maxThumb.value / state.thumbs.minThumb.value
        else
          state.thumbs.minThumb.value / state.thumbs.maxThumb.value
      f n = Int.floor (d * Int.toNumber n)
    { earlier:
        if state.earlierLastSet then state.earlier
        else
          Maybe.fromMaybe state.earlier (state.later # Format.unformat >>> map (f >>> Format.format))
    , later:
        if not state.earlierLastSet then state.later
        else
          Maybe.fromMaybe state.later (state.earlier # Format.unformat >>> map (f >>> Format.format))
    , earlierLastSet: state.earlierLastSet
    , thumbs
    }

mkAppContents :: Component InflationData
mkAppContents = do
  rangeSlider <- mkRangeSlider
  reducer <- mkReducer
  Hooks.component "AppContents" \props -> Hooks.do
    let
      keys = Array.fromFoldable (Map.keys props.allData)
      minKeyIndex = 0
      maxKeyIndex = Array.length keys - 1

      initialState :: State
      initialState =
        { earlier: "100"
        , later: ""
        , thumbs: props.thumbs
        , earlierLastSet: true
        }
    state /\ dispatch <- Hooks.useReducer initialState reducer

    pure do
      DOM.div_
        [ DOM.h1_ [ DOM.text "Inflation data" ]
        , rangeSlider
            { minValue: minKeyIndex
            , maxValue: maxKeyIndex
            , value: do
                let
                  minThumb = Array.findIndex (_ == state.thumbs.minThumb.key) keys # Maybe.fromMaybe minKeyIndex
                  maxThumb = Array.findIndex (_ == state.thumbs.maxThumb.key) keys # Maybe.fromMaybe maxKeyIndex
                { minThumb, maxThumb }
            , onChange: \{ minThumb, maxThumb } -> do
                let
                  maybeThumbs = do
                    min' <- do
                      key <- keys !! minThumb
                      value <- Map.lookup key props.allData
                      pure { key, value }
                    max' <- do
                      key <- keys !! maxThumb
                      value <- Map.lookup key props.allData
                      pure { key, value }
                    pure { minThumb: min', maxThumb: max' }
                Foldable.for_ maybeThumbs (dispatch <<< SetThumbs)
            }
        , DOM.section
            { className: "inputs"
            , children:
                [ DOM.div
                    { className: "dollar-input-group"
                    , children:
                        [ DOM.text "$"
                        , DOM.input
                            { value: state.earlier
                            , onChange: Events.handler DOM.Events.targetValue do
                                Traversable.traverse_ \value ->
                                  dispatch (Earlier value)
                            , type: "text"
                            , className: "dollar"
                            }
                        ]
                    }
                , DOM.div
                    { className: "dollar-input-group"
                    , children:
                        [ DOM.text "$"
                        , DOM.input
                            { value: state.later
                            , onChange: Events.handler DOM.Events.targetValue do
                                Traversable.traverse_ \value ->
                                  dispatch (Later value)
                            , type: "text"
                            , className: "dollar"
                            }
                        ]
                    }
                ]
            }
        ]
