module Main where

import Heart

import StartApp.Simple


app = StartApp.Simple.start
  { model = Heart.init
  , update = Heart.update
  , view = Heart.view
  }


main = app.html
