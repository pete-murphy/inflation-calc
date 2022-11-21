module App (mkApp) where

import Prelude

import Control.Monad.Except (ExceptT(..))
import Control.Monad.Except as Except
import Control.Monad.Except as ExceptT
import Data.Array ((!!))
import Data.Array as Array
import Data.Date (Month(..), Year)
import Data.Either (Either(..))
import Data.Either as Either
import Data.Enum as Enum
import Data.Foldable as Foldable
import Data.FoldableWithIndex as FoldableWithIndex
import Data.Int as Int
import Data.List.NonEmpty as List.NonEmpty
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Newtype as Newtype
import Data.Number as Number
import Data.Profunctor.Star (Star(..))
import Data.Profunctor.Strong ((&&&), (***))
import Data.String as String
import Data.String.Regex as Regex
import Data.Traversable as Traversable
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\))
import Debug as Debug
import Effect.Aff (Aff, Error)
import Effect.Aff as Aff
import Effect.Class as Effect
import Foreign (Foreign, MultipleErrors)
import Foreign.Object (Object)
import Number.Format as Format
import Prim.Row (class Union)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events as DOM.Events
import React.Basic.Events as Events
import React.Basic.Hooks (Component, (/\))
import React.Basic.Hooks as Hooks
import React.Basic.Hooks.Aff as Hooks.Aff
import Safe.Coerce as Coerce
import Slider (mkRangeSlider)
import Temp.Data as Temp.Data
import Yoga.Fetch (Credentials, Method, Redirect, Response, URL(..))
import Yoga.Fetch as Fetch
import Yoga.Fetch.Impl.Window as Fetch.Impl.Window
import Yoga.JSON as JSON
import Yoga.JSON.Error as JSON.Error

foreign import apiKey :: String

-- Note: needs to be a Tuple so that it's ordered by year _then_ month
type YearMonth = Year /\ Month

displayYearMonth :: YearMonth -> String
displayYearMonth (year /\ month) = show month <> " " <> show (Enum.fromEnum year)

type InflationData =
  { minKey :: YearMonth
  , maxKey :: YearMonth
  , allData :: Map YearMonth Number
  }

fetch
  :: forall (options :: Row Type) (trash :: Row Type)
   . Union options trash (body :: String, credentials :: Credentials, follow :: Int, headers :: Object String, method :: Method, redirect :: Redirect)
  => URL
  -> { method :: Method | options }
  -> Aff Response
fetch = Fetch.fetch Fetch.Impl.Window.windowFetch

data FetchInflationDataError
  = FetchError Error
  | JSONError Error
  | ReadError MultipleErrors
  | InvariantError'NonIntString
  | InvariantError'MapSize

printFetchInflationDataError :: FetchInflationDataError -> String
printFetchInflationDataError = case _ of
  FetchError error -> show error
  JSONError error -> show error
  ReadError errors -> String.joinWith "\n" (List.NonEmpty.toUnfoldable (JSON.Error.renderHumanError <$> errors))
  InvariantError'NonIntString -> "Invariant Error: Non-Int String"
  InvariantError'MapSize -> "Invariant Error: Map Size"

-- fetchInflationData :: ExceptT FetchInflationDataError Aff InflationData
-- fetchInflationData = do
--   response <- (Except.withExceptT FetchError <<< ExceptT <<< Aff.attempt) do
--     let
--       body = JSON.writeJSON
--         { seriesid: [ "CUUR0000SA0" ]
--         , startyear: "1913"
--         , endyear: "2022"
--         , registrationkey: apiKey
--         }
--     fetch (URL "https://api.bls.gov/publicAPI/v2/timeseries/data/") { method: Fetch.postMethod, body }

--   json <- (Except.withExceptT JSONError <<< ExceptT <<< Aff.attempt) do
--     Fetch.json response
--   (_ /\ parsedData :: Tuple Foreign (Array { date :: String, value :: Number })) <-
--     (Except.withExceptT ReadError <<< Except.except) do
--       JSON.read json
--   inflationData <- Except.except do
--     let
--       entries :: Maybe (Array (Tuple Int Number))
--       entries = Traversable.for parsedData \datum -> (_ /\ datum.value) <$> Int.fromString datum.date
--     inflationData' <- Map.fromFoldable <$> entries # Either.note InvariantError'NonIntString
--     { minKey, maxKey } <- Either.note InvariantError'MapSize do
--       min <- Map.findMin inflationData'
--       max <- Map.findMax inflationData'
--       pure { minKey: min.key, maxKey: max.key }
--     if minKey < maxKey then pure { minKey, maxKey, allData: inflationData' }
--     else Except.throwError InvariantError'MapSize

--   pure inflationData

data AppData
  = Pending
  | Failure InvariantError
  -- | Failure FetchInflationDataError
  | Success InflationData

data InvariantError
  = BadYear Int
  | BadMonth String
  | EmptyMap

printInvariantError :: InvariantError -> String
printInvariantError = case _ of
  BadYear n -> "Received bad year: " <> show n
  BadMonth m -> "Received bad month: " <> m
  EmptyMap -> "Empty map"

appData :: AppData
appData = Either.either Failure Success do
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
    -- appData /\ setAppData <- Hooks.useState' Pending
    -- Hooks.Aff.useAff unit do
    --   result <- ExceptT.runExceptT fetchInflationData
    --   Effect.liftEffect case result of
    --     Left error -> setAppData (Failure error)
    --     Right inflationData -> setAppData (Success inflationData)

    pure case appData of
      Pending -> DOM.p_ [ DOM.text "Pending..." ]
      Failure error -> DOM.pre_ [ DOM.text (printInvariantError error) ]
      Success inflationData -> appContents inflationData

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
                        (_ >>= Format.unformatInt)
                          >>> Traversable.traverse_ \value ->
                            setInput (Format.formatInt value)
                    , type: "text"
                    , className: "dollar"
                    }
                ]
            }
        , DOM.p_
            [ DOM.text do
                case Format.unformatInt input of
                  Just n -> Maybe.fromMaybe "Oops!" do
                    x <- minV
                    y <- maxV
                    pure ("$" <> Format.formatInt (Int.floor ((y / x) * Int.toNumber n)))
                  Nothing -> "Failed to parse " <> input <> " as a number"
            ]
        ]
