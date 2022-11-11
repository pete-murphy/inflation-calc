module App where

import Prelude

import Control.Monad.Except (ExceptT(..))
import Control.Monad.Except as Except
import Control.Monad.Except as ExceptT
import Data.Either (Either(..))
import Data.Either as Either
import Data.FoldableWithIndex (class FoldableWithIndex)
import Data.FoldableWithIndex as FoldableWithIndex
import Data.Int as Int
import Data.List.NonEmpty as List.NonEmpty
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.String as String
import Data.Traversable as Traversable
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, Error)
import Effect.Aff as Aff
import Effect.Class as Effect
import Foreign (Foreign, MultipleErrors)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, (/\))
import React.Basic.Hooks as Hooks
import React.Basic.Hooks.Aff as Hooks.Aff
import Yoga.Fetch (Response, URL(..))
import Yoga.Fetch as Fetch
import Yoga.Fetch.Impl.Window as Fetch.Impl.Window
import Yoga.JSON as JSON
import Yoga.JSON.Error as JSON.Error

type InflationData = Map
  Int -- Year
  Number -- Inflation rate

fetch :: URL -> Aff Response
fetch url = Fetch.fetch Fetch.Impl.Window.windowFetch url Fetch.defaultFetchOptions

data AppError
  = FetchError Error
  | JSONError Error
  | ReadError MultipleErrors
  | InvariantError

printAppError :: AppError -> String
printAppError = case _ of
  FetchError error -> show error
  JSONError error -> show error
  ReadError errors -> String.joinWith "\n" (List.NonEmpty.toUnfoldable (JSON.Error.renderHumanError <$> errors))
  InvariantError -> "InvariantError"

fetchInflationData :: ExceptT AppError Aff InflationData
fetchInflationData = do
  response <- (Except.withExceptT FetchError <<< ExceptT <<< Aff.attempt) do
    fetch (URL "http://api.worldbank.org/v2/country/us/indicator/FP.CPI.TOTL.ZG?format=json&per_page=100")
  json <- (Except.withExceptT JSONError <<< ExceptT <<< Aff.attempt) do
    Fetch.json response
  (Tuple _ parsedData :: Tuple Foreign (Array { date :: String, value :: Number })) <-
    (Except.withExceptT ReadError <<< Except.except) do
      JSON.read json
  inflationData <- (Except.except <<< Either.note InvariantError) do
    let
      entries :: Maybe (Array (Tuple Int Number))
      entries = Traversable.for parsedData \datum -> flip Tuple datum.value <$> Int.fromString datum.date
    Map.fromFoldable <$> entries
  pure inflationData

data AppData
  = Pending
  | Failure AppError
  | Success InflationData

mkApp :: Component Unit
mkApp = do
  appContents <- mkAppContents
  Hooks.component "App" \_ -> Hooks.do
    appData /\ setAppData <- Hooks.useState' Pending
    Hooks.Aff.useAff unit do
      result <- ExceptT.runExceptT fetchInflationData
      Effect.liftEffect case result of
        Left appError -> setAppData (Failure appError)
        Right inflationData -> setAppData (Success inflationData)
    pure case appData of
      Pending -> DOM.p_ [ DOM.text "Pending..." ]
      Failure appError -> DOM.pre_ [ DOM.text (printAppError appError) ]
      Success inflationData -> appContents inflationData

mkAppContents :: Component InflationData
mkAppContents = do
  Hooks.component "AppContents" \inflationData -> do
    let x = Map.foldSubmap
    pure do
      DOM.div_
        [ DOM.h1_ [ DOM.text "Inflation data" ]
        , DOM.ul_
            ( inflationData # FoldableWithIndex.foldMapWithIndex \year value ->
                [ DOM.li_ [ DOM.text (show year <> ": " <> show value) ] ]
            )
        ]
