module Position
  ( Position
  , PointedSvgElementInfo
  , pointedSvgElementInfos
  , keepSendingPointedSvgElementInfo
  ) where

import Maybe
import Mouse
import Signal exposing (Signal)
import Touch
import Task exposing (Task, andThen)

import Debug exposing (..)

import Native.Position


type alias Position = (Int, Int)

type alias ElementId = String

type alias PointedSvgElementInfo =
  { svgPosition : Position
  , id          : ElementId
  }


pointedSvgElementInfos : Signal.Mailbox PointedSvgElementInfo
pointedSvgElementInfos = Signal.mailbox <| PointedSvgElementInfo (0, 0) ""


keepSendingPointedSvgElementInfo : Signal (Task x ())
keepSendingPointedSvgElementInfo =
  Signal.map (sendPointedSvgElement pointedSvgElementInfos.address) mouseTouch


sendPointedSvgElement : Signal.Address PointedSvgElementInfo -> Maybe Position -> Task x ()
sendPointedSvgElement address maybePosition =
  getPointedSvgElement maybePosition
    `andThen` \maybeElement ->
      case maybeElement of
        Just element -> Signal.send address element
        _            -> noOp


getPointedSvgElement : Maybe Position -> Task x (Maybe PointedSvgElementInfo)
getPointedSvgElement maybePosition =
  case maybePosition of
    Just position ->
      getIdFromPoint position
        `andThen` \maybeId ->
          case maybeId of
            Just id ->
              convertViewportPointToSvgPoint position
                `andThen` \svgPosition ->
                  Task.succeed <| Just <| PointedSvgElementInfo svgPosition id
            _ ->
              Task.succeed Nothing
    _ ->
      Task.succeed Nothing


getIdFromPoint : Position -> Task x (Maybe String)
getIdFromPoint xy =
  Native.Position.getIdFromPoint xy


convertViewportPointToSvgPoint : Position -> Task x Position
convertViewportPointToSvgPoint xy =
  Native.Position.convertViewportPointToSvgPoint xy


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
