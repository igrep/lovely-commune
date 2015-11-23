module Main where

import Heart

import StartApp.Simple


main =
  StartApp.Simple.start
    { model = Heart.init
    , update = Heart.update
    , view = Heart.view
    }
