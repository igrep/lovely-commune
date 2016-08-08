module Heart.Part exposing (..)

import Svg exposing (..)
import Svg.Attributes as A
import Svg.Events as E

import Heart.Part.Constants as C
import Position exposing (Position)


type alias Model =
  { filled             : Percentage      -- how much filled by tracing
  , isPulsating        : Bool
  , id                 : Id              -- id attribute of <path>
  , positionComparator : Position -> Int -- used to compute what percentage of the part is filled when having its point touched
  , gradientEnd1       : Position        -- upper or left vertex at which the gradient starts or ends
  , gradientEnd2       : Position        -- lower or right vertex at which the gradient starts or ends
  , draw               : String          -- content of d attribute of <path>
  }

type alias Id = String

type Direction = FromTopLeft | FromBottomRight

type alias Percentage = Float


-- TODO: fill by pointed position
traceAt : Position -> Direction -> Model -> Model
traceAt _ _ m = { m | filled = C.filled }


pulsate : Model -> Model
pulsate m =
  { m | isPulsating = True, filled = C.filled }


isFilled : Model -> Bool
isFilled m = m.filled >= C.filled


viewPath : Model -> Svg
viewPath m =
  let class =
        if m.isPulsating then
          "heart-part" ++ " animated flash"
        else
          "heart-part"
  in
  path
    [ A.class class
    , A.d m.draw
    , A.id m.id
    , A.style <| "fill: url(#" ++ (gradientId m) ++ ")"
    ]
    []


viewGradient : Model -> Svg
viewGradient m =
  linearGradient
    [ A.spreadMethod "pad"
    , A.gradientUnits "userSpaceOnUse"
    , A.x1 <| toString <| axisX m.gradientEnd1
    , A.y1 <| toString <| axisY m.gradientEnd1
    , A.x2 <| toString <| axisX m.gradientEnd2
    , A.y2 <| toString <| axisY m.gradientEnd2
    , A.id <| gradientId m
    ]
    -- TODO: Switch when traced by FromBottomRight
    [ stop
        [ A.class "heart-gradiationStop heart-gradiationStop--traced_true"
        , A.offset <| toString m.filled
        ]
        []
    , stop
        [ A.class "heart-gradiationStop heart-gradiationStop--traced_false"
        , A.offset <| toString m.filled
        ]
        []
    ]


gradientId : Model -> Id
gradientId m = "gradient-" ++ m.id


stopId : Model -> Id
stopId m = "gradientStop-" ++ m.id


initModels : List Model
initModels =
  [ initLeft
  , initRight
  , initUpperLeft
  , initUpperRight
  , initBottomLeft
  , initBottomRight
  , initCenterLeft
  , initCenterRight
  ]


init : Id -> (Position -> Int) -> Position -> Position -> String -> Model
init = Model C.empty False


initLeft : Model
initLeft = init C.left axisY (78, 73) (123, 395) "m 77.832545,73.499417 c -36.282713,33.405363 -56.47197,71.961443 -57,131.000003 1,82 57,136 102.303875,190.34806 19.31275,-58.94291 20.69612,-118.34806 13.37467,-178.9431 -5.77813,-47.82183 -25.61233,-94.70957 -58.678545,-142.404963 z"


initRight : Model
initRight = init C.right axisY (501, 73) (457, 395) "m 502.20905,72.801742 c 36.28271,33.405368 56.47196,71.961448 56.99999,131.000008 -1,82 -56.99999,136 -102.30387,190.34806 -19.31275,-58.94291 -20.69612,-118.34806 -13.37467,-178.9431 5.77813,-47.82183 25.61233,-94.70957 58.67855,-142.404968 z"

initUpperLeft : Model
initUpperLeft = init C.upperLeft axisX (88, 100) (291, 100) "m 290.83255,35.499417 c -22.00001,-12 -63.00001,-17 -102.67112,-12.579008 -32.10606,3.577934 -63.32889,17.579008 -99.049567,41.365788 23.458807,34.093405 44.232597,70.404873 54.720687,115.213223 35.99999,-45 92.31217,-65.17868 147,-35"


initUpperRight : Model
initUpperRight = init C.upperRight axisX (290, 100) (492, 100) "m 289.9722,35.499417 c 22.00001,-12 63.00001,-17 102.67112,-12.579008 32.10606,3.577934 63.32889,17.579008 99.04957,41.365788 -23.45881,34.093405 -44.2326,70.404873 -54.72069,115.213223 -35.99999,-45 -92.31217,-65.17868 -147,-35"


initBottomLeft : Model
initBottomLeft = init C.bottomLeft axisX (149, 340) (289, 517) "m 290.83255,433.62936 c -60.00001,-30.12994 -111.77329,-81.52088 -138,-136.12994 -3.7496,37.3272 -4.1263,76.98949 -17,108 21.77107,19.87863 38.23442,34.67652 55.1925,47.21311 51.98861,38.4336 66.80749,46.92907 99.8075,64.78689"


initBottomRight : Model
initBottomRight = init C.bottomRight axisX (290, 407) (446, 407) "m 290.33254,433.62936 c 60.00001,-30.12994 111.77329,-81.52088 138,-136.12994 3.7496,37.3272 4.1263,76.98949 17,108 -21.77107,19.87863 -38.23442,34.67652 -55.1925,47.21311 -51.98861,38.4336 -66.80749,46.92907 -99.8075,64.78689"


initCenterLeft : Model
initCenterLeft = init C.centerLeft axisX (155, 273) (291, 273) "m 290.83255,234.49942 c -13.00001,-19 -45.00137,-26.07131 -65.3123,-25.64765 -40.9411,0.85398 -67.18703,18.45477 -70.0893,33.44685 -0.59841,38.2008 71.1193,69.2008 135.4016,95.2008"


initCenterRight : Model
initCenterRight = init C.centerRight axisX (290, 273) (426, 273) "m 290.12989,234.49942 c 13.00001,-19 45.00137,-26.07131 65.3123,-25.64765 40.9411,0.85398 67.18703,18.45477 70.0893,33.44685 0.59841,38.2008 -71.1193,69.2008 -135.4016,95.2008"


axisX : Position -> Int
axisX = fst


axisY : Position -> Int
axisY = snd
