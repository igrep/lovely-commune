module Heart.Part where

import Svg exposing (..)
import Svg.Attributes as A
import Svg.Events as E


type alias Model =
  { id      : Id               -- id attribute of <path>
  , filled  : Percentage       -- how much filled by tracing
  , readyTo : Maybe Direction  -- which direction to trace from
  , draw    : String           -- content of d attribute of <path>
  }

type alias Id = String

type Direction = FromTopLeft | FromBottomRight

type alias Percentage = Float


fill : Float -> Model -> Model
fill percentage m = { m | filled = percentage }


view : Model -> Svg
view m =
  let className =
        if m.filled == 100
          then "heart-part heart-part--filled_true"
          else "heart-part heart-part--filled_false"
  in
  path [A.class className, A.d m.draw, A.id m.id] []


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


init : Id -> String -> Model
init id draw = Model id 0 Nothing draw


initLeft : Model
initLeft = init "left" "m 77.832545,73.499417 c -36.282713,33.405363 -56.47197,71.961443 -57,131.000003 1,82 57,136 102.303875,190.34806 19.31275,-58.94291 20.69612,-118.34806 13.37467,-178.9431 -5.77813,-47.82183 -25.61233,-94.70957 -58.678545,-142.404963 z"


initRight : Model
initRight = init "right" "m 502.20905,72.801742 c 36.28271,33.405368 56.47196,71.961448 56.99999,131.000008 -1,82 -56.99999,136 -102.30387,190.34806 -19.31275,-58.94291 -20.69612,-118.34806 -13.37467,-178.9431 5.77813,-47.82183 25.61233,-94.70957 58.67855,-142.404968 z"

initUpperLeft : Model
initUpperLeft = init "upperLeft" "m 290.83255,35.499417 c -22.00001,-12 -63.00001,-17 -102.67112,-12.579008 -32.10606,3.577934 -63.32889,17.579008 -99.049567,41.365788 23.458807,34.093405 44.232597,70.404873 54.720687,115.213223 35.99999,-45 92.31217,-65.17868 147,-35"


initUpperRight : Model
initUpperRight = init "upperRight" "m 289.9722,35.499417 c 22.00001,-12 63.00001,-17 102.67112,-12.579008 32.10606,3.577934 63.32889,17.579008 99.04957,41.365788 -23.45881,34.093405 -44.2326,70.404873 -54.72069,115.213223 -35.99999,-45 -92.31217,-65.17868 -147,-35"


initBottomLeft : Model
initBottomLeft = init "bottomLeft" "m 290.83255,433.62936 c -60.00001,-30.12994 -111.77329,-81.52088 -138,-136.12994 -3.7496,37.3272 -4.1263,76.98949 -17,108 21.77107,19.87863 38.23442,34.67652 55.1925,47.21311 51.98861,38.4336 66.80749,46.92907 99.8075,64.78689"


initBottomRight : Model
initBottomRight = init "bottomRight" "m 290.33254,433.62936 c 60.00001,-30.12994 111.77329,-81.52088 138,-136.12994 3.7496,37.3272 4.1263,76.98949 17,108 -21.77107,19.87863 -38.23442,34.67652 -55.1925,47.21311 -51.98861,38.4336 -66.80749,46.92907 -99.8075,64.78689"


initCenterLeft : Model
initCenterLeft = init "centerLeft" "m 290.83255,234.49942 c -13.00001,-19 -45.00137,-26.07131 -65.3123,-25.64765 -40.9411,0.85398 -67.18703,18.45477 -70.0893,33.44685 -0.59841,38.2008 71.1193,69.2008 135.4016,95.2008"


initCenterRight : Model
initCenterRight = init "centerRight" "m 290.12989,234.49942 c 13.00001,-19 45.00137,-26.07131 65.3123,-25.64765 40.9411,0.85398 67.18703,18.45477 70.0893,33.44685 0.59841,38.2008 -71.1193,69.2008 -135.4016,95.2008"
