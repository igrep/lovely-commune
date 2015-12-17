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


type alias Model =
  { heartState     : HeartState
  , leftDirections : List (Maybe Direction)
  }

type alias HeartState = Dict Part.Id Part.Model

type Action = Trace Position.PointedElementInfo | Reset

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
  [ Direction "left"        Part.FromTopLeft
  , Direction "bottomLeft"  Part.FromTopLeft
  , Direction "bottomRight" Part.FromTopLeft
  ]


cO : List Direction
cO =
  [ Direction "bottomLeft"  Part.FromBottomRight
  , Direction "left"        Part.FromBottomRight
  , Direction "upperLeft"   Part.FromTopLeft
  , Direction "upperRight"  Part.FromTopLeft
  , Direction "right"       Part.FromTopLeft
  , Direction "bottomRight" Part.FromBottomRight
  ]


cV : List Direction
cV =
  [ Direction "left"        Part.FromTopLeft
  , Direction "bottomLeft"  Part.FromTopLeft
  , Direction "bottomRight" Part.FromTopLeft
  , Direction "right"       Part.FromBottomRight
  ]


cE : List Direction
cE =
  [ Direction "upperRight"  Part.FromTopLeft
  , Direction "upperLeft"   Part.FromTopLeft
  , Direction "left"        Part.FromTopLeft
  , Direction "centerLeft"  Part.FromTopLeft
  , Direction "centerRight" Part.FromTopLeft
  , Direction "bottomLeft"  Part.FromTopLeft
  , Direction "bottomRight" Part.FromTopLeft
  ]


directionStack : List (Maybe Direction)
directionStack =
  join [Nothing] <| List.map (List.map Just) [cL, cO, cV, cE]


update : Action -> Model -> (Model, Effects Action)
update a m =
  case m.leftDirections of
    current :: left ->
      case a of
        Reset ->
          ( { m
              | heartState = turnedOffHeartState
            }
          , Effects.none
          )
        Trace pointedElement ->
          let tracedM =
                case current of
                  Just direction ->
                    if pointedElement.id == direction.targetId then
                      { m
                      | heartState = tracePart pointedElement.id m.heartState
                      , leftDirections = left
                      }
                    else
                      m
                  _ ->
                    { m | leftDirections = left }
          in
          case left of
            Nothing :: _ ->
              ( tracedM, resetEventually )
            _ ->
              ( tracedM, Effects.none )
    [] ->
      ( m, Effects.none )


tracePart : Part.Id -> HeartState -> HeartState
tracePart id heartState =
  Dict.update id (Maybe.map (Part.fill 100)) heartState


resetEventually : Effects Action
resetEventually =
  Effects.task <| Task.sleep 600 `andThen` \_ -> Task.succeed Reset


view : Signal.Address Action -> Model -> Html
view _ m =
    svg
      [ A.version "1.1"
      , A.width   "100%"
      , A.height  "100%"
      , A.viewBox "0 0 574.49699 527.37143"
      ]
      (Dict.values m.heartState |> List.map Part.view)


join : List a -> List (List a) -> List a
join x = List.intersperse x >> List.concat
