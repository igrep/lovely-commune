module Position
  ( mouseTouch
  , Position
  , pointedElementId
  ) where

import Mouse
import Signal exposing (Signal)
import Touch
import Task exposing (Task, andThen)

import Native.Position


type alias Position = Maybe (Int, Int)


mouseTouch : Signal Position
mouseTouch =
  let mouse = Signal.map Just Mouse.position
      touch =
        Signal.map (Maybe.map toPair << List.head) Touch.touches
  in
  Signal.merge mouse touch


pointedElementId : Signal.Address String -> Signal (Task x ())
pointedElementId address =
  Signal.map (sendPointedElementId address) mouseTouch


sendPointedElementId : Signal.Address String -> Position -> Task x ()
sendPointedElementId address position =
  getIdFromPoint position
    `andThen` \maybeId ->
      case maybeId of
        Just id -> Signal.send address id
        _ -> Task.succeed ()


toPair : Touch.Touch -> (Int, Int)
toPair t = (t.x, t.y)


getIdFromPoint : Position -> Task x (Maybe String)
getIdFromPoint position =
  case position of
    Just (x, y) -> Native.Position.getIdFromPoint x y
    _ -> Task.succeed Nothing
