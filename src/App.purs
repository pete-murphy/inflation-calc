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
import Partial.Unsafe as Unsafe
import React.Basic.DOM as DOM
import React.Basic.DOM.Events as DOM.Events
import React.Basic.DOM.SVG as SVG
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
        { earlier: ""
        , later: ""
        , thumbs: props.thumbs
        , earlierLastSet: true
        }
    state /\ dispatch <- Hooks.useReducer initialState reducer

    arr <- Hooks.useMemo unit \_ -> Array.fromFoldable props.allData

    -- TODO: Responsive
    let w = 360
    let h = 450
    let
      inflData' :: Array Number
      inflData' = Array.zipWith sub arr (Array.drop 1 arr)
    let
      inflData = do
        let x' = Array.head inflData' # Maybe.fromMaybe' \_ -> Unsafe.unsafeCrashWith "Oops, no head"
        let y' = Array.last inflData' # Maybe.fromMaybe' \_ -> Unsafe.unsafeCrashWith "Oops, no last"
        let xs = Array.cons x' inflData'
        let ys = Array.snoc inflData' y'
        let zs = Array.zipWith ($) (Array.zipWith (\a b c -> a + b + c / 3.0) xs ys) inflData'
        zs
    let l = Array.length inflData
    let w' = Int.toNumber w / Int.toNumber l
    let mn'' = Foldable.minimum inflData # Maybe.fromMaybe' \_ -> Unsafe.unsafeCrashWith "Oops, no min"
    let
      mx = Foldable.maximum inflData # Maybe.fromMaybe' \_ -> Unsafe.unsafeCrashWith "Oops, no max"
      mn = negate mx
    let h' = Int.toNumber h
    let
      sc :: Number -> Number
      sc y = do
        let x = (y - mn) / (mx - mn)
        (1.0 - x) * h'
    let
      minThumbIndex = Array.findIndex (_ == state.thumbs.minThumb.key) keys # Maybe.fromMaybe minKeyIndex
      maxThumbIndex = Array.findIndex (_ == state.thumbs.maxThumb.key) keys # Maybe.fromMaybe maxKeyIndex
    let
      ds = inflData # Array.mapWithIndex \i d -> do
        let
          isNegative = d < 0.0
          y = if isNegative then sc 0.0 else sc d
          height = if isNegative then sc d - sc 0.0 else sc 0.0 - sc d
          fill = if isNegative then "tomato" else "black"
        { x: show (w' * Int.toNumber i)
        , width: show w'
        , fill
        , height: show (height)
        , y: show y
        , opacity:
            if between minThumbIndex maxThumbIndex i then "1"
            else "0.2"
        }

    Hooks.useEffectOnce do
      let
        -- Random thumbs to get things started
        maybeThumbs = do
          min' <- do
            key <- keys !! 800
            value <- Map.lookup key props.allData
            pure { key, value }
          max' <- do
            key <- keys !! 1200
            value <- Map.lookup key props.allData
            pure { key, value }
          pure { minThumb: min', maxThumb: max' }
      Traversable.for_ maybeThumbs (dispatch <<< SetThumbs)

      dispatch (Earlier "100")

      pure mempty

    pure do
      DOM.div_
        [ DOM.section
            { className: "slider-section"
            , children:
                [ rangeSlider
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
                , SVG.svg
                    { height: show h
                    , width: show w
                    , children: ds <#> \d -> SVG.rect d
                    }
                ]
            }

        , DOM.section
            { className: "outputs-keys"
            , children:
                [ DOM.output_
                    [ DOM.text (displayYearMonth state.thumbs.minThumb.key)
                    ]
                , DOM.output_
                    [ DOM.text (displayYearMonth state.thumbs.maxThumb.key)
                    ]
                ]
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
                            , inputMode: "numeric"
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
                            , inputMode: "numeric"
                            }
                        ]
                    }
                ]
            }
        ]

onNothingM :: forall m a. Monad m => m a -> m (Maybe a) -> m a
onNothingM ma = (_ >>= Maybe.maybe ma pure)

whenNothingM :: forall m a. Monad m => m (Maybe a) -> m a -> m a
whenNothingM = flip onNothingM

onNothing :: forall m a. Monad m => m a -> Maybe a -> m a
onNothing ma = Maybe.maybe ma pure

whenNothing :: forall m a. Monad m => Maybe a -> m a -> m a
whenNothing = flip onNothing