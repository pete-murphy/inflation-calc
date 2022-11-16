module App where

import Prelude

import Control.Monad.Except (ExceptT(..))
import Control.Monad.Except as Except
import Control.Monad.Except as ExceptT
import Data.Either (Either(..))
import Data.Either as Either
import Data.FoldableWithIndex as FoldableWithIndex
import Data.Int as Int
import Data.List.NonEmpty as List.NonEmpty
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Maybe as Maybe
import Data.String as String
import Data.Traversable as Traversable
import Data.Tuple (Tuple)
import Effect.Aff (Aff, Error)
import Effect.Aff as Aff
import Effect.Class as Effect
import Foreign (Foreign, MultipleErrors)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, (/\))
import React.Basic.Hooks as Hooks
import React.Basic.Hooks.Aff as Hooks.Aff
import Slider (mkRangeSlider)
import Yoga.Fetch (Response, URL(..))
import Yoga.Fetch as Fetch
import Yoga.Fetch.Impl.Window as Fetch.Impl.Window
import Yoga.JSON as JSON
import Yoga.JSON.Error as JSON.Error

type NonEmptyMap k v = Tuple { key :: k, value :: v } (Map k v)

type InflationData =
  { minKey :: Int
  , maxKey :: Int
  , allData :: Map Int Number
  }

fetch :: URL -> Aff Response
fetch url = Fetch.fetch Fetch.Impl.Window.windowFetch url Fetch.defaultFetchOptions

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

fetchInflationData :: ExceptT FetchInflationDataError Aff InflationData
fetchInflationData = do
  response <- (Except.withExceptT FetchError <<< ExceptT <<< Aff.attempt) do
    fetch (URL "http://api.worldbank.org/v2/country/us/indicator/FP.CPI.TOTL.ZG?format=json&per_page=100")
  json <- (Except.withExceptT JSONError <<< ExceptT <<< Aff.attempt) do
    Fetch.json response
  (_ /\ parsedData :: Tuple Foreign (Array { date :: String, value :: Number })) <-
    (Except.withExceptT ReadError <<< Except.except) do
      JSON.read json
  inflationData <- Except.except do
    let
      entries :: Maybe (Array (Tuple Int Number))
      entries = Traversable.for parsedData \datum -> (_ /\ datum.value) <$> Int.fromString datum.date
    inflationData' <- Map.fromFoldable <$> entries # Either.note InvariantError'NonIntString
    minKey <- Map.findMin inflationData' <#> _.key # Either.note InvariantError'MapSize
    maxKey <- Map.findMax inflationData' <#> _.key # Either.note InvariantError'MapSize
    if minKey < maxKey then pure { minKey, maxKey, allData: inflationData' }
    else Except.throwError InvariantError'MapSize

  pure inflationData

data AppData
  = Pending
  | Failure FetchInflationDataError
  | Success InflationData

mkApp :: Component Unit
mkApp = do
  appContents <- mkAppContents
  Hooks.component "App" \_ -> Hooks.do
    appData /\ setAppData <- Hooks.useState' Pending
    Hooks.Aff.useAff unit do
      result <- ExceptT.runExceptT fetchInflationData
      Effect.liftEffect case result of
        Left error -> setAppData (Failure error)
        Right inflationData -> setAppData (Success inflationData)
    pure case appData of
      Pending -> DOM.p_ [ DOM.text "Pending..." ]
      Failure error -> DOM.pre_ [ DOM.text (printFetchInflationDataError error) ]
      Success inflationData -> appContents inflationData

mkAppContents :: Component InflationData
mkAppContents = do
  rangeSlider <- mkRangeSlider
  Hooks.component "AppContents" \props -> Hooks.do
    thumbs /\ setThumbs <- Hooks.useState' { minThumb: props.minKey, maxThumb: props.maxKey }

    pure do
      DOM.div_
        [ DOM.h1_ [ DOM.text "Inflation data" ]
        , rangeSlider
            { minValue: props.minKey
            , maxValue: props.maxKey
            , value: thumbs
            , onChange: setThumbs
            }
        -- , DOM.ul_
        --     ( inflationData # FoldableWithIndex.foldMapWithIndex \year value ->
        --         [ DOM.li_ [ DOM.text (show year <> ": " <> show value) ] ]
        --     )
        ]
