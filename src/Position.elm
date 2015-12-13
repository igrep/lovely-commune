module Position
  ( mouseTouch
  , Position
  , PointedElementInfo
  , pointedElementId
  ) where

import Maybe
import Mouse
import Signal exposing (Signal)
import Touch
import Task exposing (Task, andThen)

import Native.Position


type alias Position = (Int, Int)

type alias ElementId = String

type alias PointedElementInfo =
  { position  : Position
  , elementId : ElementId
  }


pointedElementId : Signal.Address PointedElementInfo -> Signal (Task x ())
pointedElementId address =
  Signal.map (sendPointedElement address) mouseTouch


sendPointedElement : Signal.Address PointedElementInfo -> Maybe Position -> Task x ()
sendPointedElement address maybePosition =
  getPointedElement maybePosition
    `andThen` \maybeElement ->
      case maybeElement of
        Just element -> Signal.send address element
        _            -> noOp


getPointedElement : Maybe Position -> Task x (Maybe PointedElementInfo)
getPointedElement maybePosition =
  case maybePosition of
    Just position ->
      (Task.map << Maybe.map)
        (PointedElementInfo position)
        (getIdFromPoint position)
    _ ->
      Task.succeed Nothing


getIdFromPoint : Position -> Task x (Maybe String)
getIdFromPoint (x, y) =
  Native.Position.getIdFromPoint x y


mouseTouch : Signal (Maybe Position)
mouseTouch =
  let mouse = Signal.map Just Mouse.position
      touch =
        Signal.map (Maybe.map toPair << List.head) Touch.touches
  in
  Signal.merge mouse touch


toPair : Touch.Touch -> (Int, Int)
toPair t =
  (t.x, t.y)


noOp : Task x ()
noOp =
  Task.succeed ()
