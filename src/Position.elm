module Position
  ( mouseTouch
  , Position
  ) where

import Mouse
import Signal exposing (Signal)
import Touch


type alias Position = Maybe (Int, Int)


mouseTouch : Signal Position
mouseTouch =
  let mouse = Signal.map Just Mouse.position
      touch =
        Signal.map (Maybe.map toPair << List.head) Touch.touches
  in
  Signal.merge mouse touch


toPair : Touch.Touch -> (Int, Int)
toPair t = (t.x, t.y)
