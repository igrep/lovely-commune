module Heart
  ( init
  , update
  , view
  , Action(..)
  ) where

import Debug exposing (..)
import Dict exposing (Dict)
import Effects exposing (Effects, Never)
import Maybe
import Html exposing (Html)
import Signal
import Svg exposing (..)
import Svg.Attributes as A
import Svg.Events as E
import Task exposing (andThen)

import Position
import Heart.Part as Part
import Heart.Part.Constants as C


type alias Model =
  { heartState     : HeartState
  , leftDirections : List (Maybe Direction)
  }

type alias HeartState = Dict Part.Id Part.Model

type Action =
  Trace Position.PointedSvgElementInfo | Reset | Complete

type alias Direction =
  { targetId  : Part.Id
  , traceFrom : Part.Direction
  }


init : (Model, Effects Action)
init =
  (
    { heartState     = turnedOffHeartState
    , leftDirections = directionStack
    }
  , Effects.none
  )


turnedOffHeartState : HeartState
turnedOffHeartState =
  Part.initModels |> List.map (\m -> (m.id, m)) |> Dict.fromList


cL : List Direction
cL =
  [ Direction C.left        Part.FromTopLeft
  , Direction C.bottomLeft  Part.FromTopLeft
  , Direction C.bottomRight Part.FromTopLeft
  ]


cO : List Direction
cO =
  [ Direction C.bottomLeft  Part.FromBottomRight
  , Direction C.left        Part.FromBottomRight
  , Direction C.upperLeft   Part.FromTopLeft
  , Direction C.upperRight  Part.FromTopLeft
  , Direction C.right       Part.FromTopLeft
  , Direction C.bottomRight Part.FromBottomRight
  ]


cV : List Direction
cV =
  [ Direction C.left        Part.FromTopLeft
  , Direction C.bottomLeft  Part.FromTopLeft
  , Direction C.bottomRight Part.FromTopLeft
  , Direction C.right       Part.FromBottomRight
  ]


cE : List Direction
cE =
  [ Direction C.upperLeft   Part.FromTopLeft
  , Direction C.upperRight  Part.FromTopLeft
  , Direction C.left        Part.FromTopLeft
  , Direction C.centerLeft  Part.FromTopLeft
  , Direction C.centerRight Part.FromTopLeft
  , Direction C.bottomLeft  Part.FromTopLeft
  , Direction C.bottomRight Part.FromTopLeft
  ]


directionStack : List (Maybe Direction)
directionStack =
  join [Nothing] <| List.map (List.map Just) [cL, cO, cV, cE]


update : Action -> Model -> (Model, Effects Action)
update a m =
  case a of
    Reset ->
      ( { m | heartState = turnedOffHeartState }
      , Effects.none
      )
    Trace pointedElement ->
      case m.leftDirections of
        current :: left ->
          case current of
            Just direction ->
              if pointedElement.id == direction.targetId then
                let tracedM =
                      tracePart pointedElement direction.traceFrom  m
                in
                case left of
                  Nothing :: _ ->
                    (tracedM, resetEventually)
                  _ ->
                    (tracedM, Effects.none)
              else
                (m, Effects.none)
            _ ->
              ( { m | leftDirections = left }
              , Effects.none
            )
        [] ->
          (m, pulateEventually)
    Complete ->
      (pulsateCenter m, Effects.none)


tracePart : Position.PointedSvgElementInfo -> Part.Direction -> Model -> Model
tracePart pointedSvgElementInfo traceFrom m =
  let pointedId = pointedSvgElementInfo.id
      tracedM =
        { m
        | heartState =
          Dict.update
            pointedId
            (Maybe.map (Part.traceAt pointedSvgElementInfo.svgPosition traceFrom))
            m.heartState
        }
  in
  case Dict.get pointedId tracedM.heartState of
    Just part ->
      if Part.isFilled part then
        popDirection tracedM
      else
        tracedM
    _ ->
      tracedM


popDirection : Model -> Model
popDirection m =
  case m.leftDirections of
    (_::left) ->
      { m | leftDirections = left }
    _ ->
      m


pulsateCenter : Model -> Model
pulsateCenter m =
  let m' = { m | heartState = turnedOffHeartState }
      m'' = { m' | heartState = Dict.update C.centerLeft (Maybe.map Part.pulsate) m'.heartState }
  in
  { m'' | heartState = Dict.update C.centerRight (Maybe.map Part.pulsate) m''.heartState }


resetEventually : Effects Action
resetEventually =
  affectEventually 600 Reset


pulateEventually : Effects Action
pulateEventually =
  affectEventually 900 Complete


affectEventually : Float -> Action -> Effects Action
affectEventually duration a =
  Effects.task <| Task.sleep duration `andThen` \_ -> Task.succeed a


view : Signal.Address Action -> Model -> Html
view _ m =
  let parts = Dict.values m.heartState
      contents =
        [ defs [] (List.map Part.viewGradient parts)
        ] ++ (List.map Part.viewPath parts)
  in
  svg
    [ A.version "1.1"
    , A.width   "100%"
    , A.height  "100%"
    , A.viewBox "0 0 574.49699 527.37143"
    ]
    contents


join : List a -> List (List a) -> List a
join x = List.intersperse x >> List.concat
