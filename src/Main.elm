module Main (main) where

import LovelyCommune
import Position

import Effects exposing (Never)
import Html exposing (Html)
import Task exposing (Task)
import Signal

import StartApp

import Debug exposing (..)


app : StartApp.App LovelyCommune.Model
app =
  StartApp.start
    { init = LovelyCommune.init
    , update = LovelyCommune.update
    , view = LovelyCommune.view
    , inputs = LovelyCommune.inputs ++ [hasShoutedPrecureLoveLink]
    }


main : Signal Html
main = app.html


port keepSendingPointedElementInfo : Signal (Task x ())
port keepSendingPointedElementInfo =
  Position.keepSendingPointedSvgElementInfo


port tasks : Signal (Task Never ())
port tasks = app.tasks


port precureLoveLink : Signal Bool


hasShoutedPrecureLoveLink : Signal LovelyCommune.Action
hasShoutedPrecureLoveLink =
  Signal.map (always LovelyCommune.PrecureLoveLink) <| Signal.filter identity False precureLoveLink
