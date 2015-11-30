module Main (main) where

import Heart

import Effects exposing (Never)
import Task exposing (Task)
import Signal

import StartApp


app =
  StartApp.start
    { init = Heart.init
    , update = Heart.update
    , view = Heart.view
    , inputs = []
    }


main = app.html


port tasks : Signal (Task Never ())
port tasks = app.tasks
