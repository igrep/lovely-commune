module LovelyCommune
  ( init
  , update
  , view
  , inputs
  , Model
  , Action(..)
  ) where

import Debug exposing (..)
import Effects exposing (Effects)
import Html exposing (..)
import Regex exposing (Regex, regex)
import Signal exposing (Signal, Address)

import LovelyCommune.Heart as Heart


type alias Model =
  Maybe Heart.Model

type Action =
  Perform Heart.Action | PrecureLoveLink


init : (Model, Effects Action)
init = (Nothing, Effects.none)


update : Action -> Model -> (Model, Effects Action)
update a m =
  case a of
    Perform ha ->
      case m of
        Just heart ->
          let (heart, effect) = Heart.update ha heart
          in
          (Just heart, Effects.map Perform effect)
        _ ->
          crash "Assertion failure: traced not ready heart!"
    PrecureLoveLink ->
      (Just Heart.init, Effects.none)


view : Address Action -> Model -> Html
view a m =
  case m of
    Just h -> Heart.view (Signal.forwardTo a Perform) h
    _      -> text "「プリキュア！ラブリンク！」と叫んで、LOVEを描くシャル！"


inputs : List (Signal Action)
inputs =
  [Signal.map Perform Heart.traces]
