module Main (main) where

import Heart
import Position exposing (pointedSvgElementInfos)

import Effects exposing (Never)
import Task exposing (Task)
import Signal

import StartApp

import Debug exposing (..)


app =
  StartApp.start
    { init = Heart.init
    , update = Heart.update
    , view = Heart.view
    , inputs = [traces]
    }


main = app.html


traces : Signal Heart.Action
traces = Signal.map Heart.Trace pointedSvgElementInfos.signal


port keepSendingPointedElementInfo : Signal (Task x ())
port keepSendingPointedElementInfo =
  Position.keepSendingPointedSvgElementInfo


port tasks : Signal (Task Never ())
port tasks = app.tasks
