module Heart where

import Debug exposing (..)
import Html exposing (Html)
import Signal
import StartApp.Simple
import Svg exposing (..)
import Svg.Attributes as A


type alias Model =
  { heartState  : HeartState
  , leftActions : List Action
  }


type alias HeartState =
  { left        : PartState
  , right       : PartState
  , upperLeft   : PartState
  , upperRight  : PartState
  , bottomLeft  : PartState
  , bottomRight : PartState
  , centerLeft  : PartState
  , centerRight : PartState
  }


type Part =
    Left       | Right
  | UpperLeft  | UpperRight
  | BottomLeft | BottomRight
  | CenterLeft | CenterRight


type PartState = Off | Ready | On


type Action = Reset | Trace Part


init : Model
init =
  { heartState  = turnedOffHeartState
  , leftActions = actionStack
  }


turnedOffHeartState : HeartState
turnedOffHeartState =
  { left        = Off
  , right       = Off
  , upperLeft   = Off
  , upperRight  = Off
  , bottomLeft  = Off
  , bottomRight = Off
  , centerLeft  = Off
  , centerRight = Off
  }


cL : List Part
cL =
  [ Left
  , BottomLeft
  , BottomRight
  ]


cO : List Part
cO =
  [ BottomLeft
  , Left
  , UpperLeft
  , UpperRight
  , Right
  , BottomRight
  ]


cV : List Part
cV =
  [ Left
  , BottomLeft
  , BottomRight
  , Right
  ]


cE : List Part
cE =
  [ UpperRight
  , UpperLeft
  , Left
  , CenterLeft
  , CenterRight
  , BottomLeft
  , BottomRight
  ]


actionStack : List Action
actionStack =
  join [Reset] <| List.map ( List.map Trace ) [cL, cO, cV, cE]


update : Action -> Model -> Model
update a m =
  case m.leftActions of
    next :: left ->
      case a of
        Reset ->
          { m
            | heartState = turnedOffHeartState
            , leftActions = left
          }
        Trace part ->
          { m
            | heartState = tracePart part m.heartState
            , leftActions = left
          }
    [] ->
      m


tracePart : Part -> HeartState -> HeartState
tracePart p hs =
  case p of
    Left        -> { hs | left        = On }
    Right       -> { hs | right       = On }
    UpperLeft   -> { hs | upperLeft   = On }
    UpperRight  -> { hs | upperRight  = On }
    BottomLeft  -> { hs | bottomLeft  = On }
    BottomRight -> { hs | bottomRight = On }
    CenterLeft  -> { hs | centerLeft  = On }
    CenterRight -> { hs | centerRight = On }


view : Signal.Address Action -> Model -> Html
view a m =
  let maybeNext = List.head m.leftActions

      left =
        viewPart "path4167" "m 77.832545,73.499417 c -36.282713,33.405363 -56.47197,71.961443 -57,131.000003 1,82 57,136 102.303875,190.34806 19.31275,-58.94291 20.69612,-118.34806 13.37467,-178.9431 -5.77813,-47.82183 -25.61233,-94.70957 -58.678545,-142.404963 z"

      right =
        viewPart "path4169" "m 502.20905,72.801742 c 36.28271,33.405368 56.47196,71.961448 56.99999,131.000008 -1,82 -56.99999,136 -102.30387,190.34806 -19.31275,-58.94291 -20.69612,-118.34806 -13.37467,-178.9431 5.77813,-47.82183 25.61233,-94.70957 58.67855,-142.404968 z"

      centerLeft =
        viewPart "path4171" "m 290.83255,234.49942 c -13.00001,-19 -45.00137,-26.07131 -65.3123,-25.64765 -40.9411,0.85398 -67.18703,18.45477 -70.0893,33.44685 -0.59841,38.2008 71.1193,69.2008 135.4016,95.2008"

      centerRight =
        viewPart "path4181" "m 290.12989,234.49942 c 13.00001,-19 45.00137,-26.07131 65.3123,-25.64765 40.9411,0.85398 67.18703,18.45477 70.0893,33.44685 0.59841,38.2008 -71.1193,69.2008 -135.4016,95.2008"

      upperLeft =
        viewPart "path4183" "m 290.83255,35.499417 c -22.00001,-12 -63.00001,-17 -102.67112,-12.579008 -32.10606,3.577934 -63.32889,17.579008 -99.049567,41.365788 23.458807,34.093405 44.232597,70.404873 54.720687,115.213223 35.99999,-45 92.31217,-65.17868 147,-35"

      upperRight =
        viewPart "path4185" "m 289.9722,35.499417 c 22.00001,-12 63.00001,-17 102.67112,-12.579008 32.10606,3.577934 63.32889,17.579008 99.04957,41.365788 -23.45881,34.093405 -44.2326,70.404873 -54.72069,115.213223 -35.99999,-45 -92.31217,-65.17868 -147,-35"

      bottomLeft =
        viewPart "path4189" "m 290.83255,433.62936 c -60.00001,-30.12994 -111.77329,-81.52088 -138,-136.12994 -3.7496,37.3272 -4.1263,76.98949 -17,108 21.77107,19.87863 38.23442,34.67652 55.1925,47.21311 51.98861,38.4336 66.80749,46.92907 99.8075,64.78689"

      bottomRight =
        viewPart "path4191" "m 290.33254,433.62936 c 60.00001,-30.12994 111.77329,-81.52088 138,-136.12994 3.7496,37.3272 4.1263,76.98949 17,108 -21.77107,19.87863 -38.23442,34.67652 -55.1925,47.21311 -51.98861,38.4336 -66.80749,46.92907 -99.8075,64.78689"


  in
    svg
      [ A.version "1.1"
      , A.width "574.49701"
      , A.height "527.37146"
      , A.viewBox "0 0 574.49699 527.37143"
      ]
      [ left       , right
      , upperLeft  , upperRight
      , bottomLeft , bottomRight
      , centerLeft , centerRight
      ]


viewPart : String -> String -> Svg
viewPart svgId svgD =
  let partStyle =
        A.style "fill:none;fill-rule:evenodd;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1"
  in
  path [partStyle, A.d svgD, A.id svgId] []


join : List a -> List (List a) -> List a
join x = List.intersperse x >> List.concat
