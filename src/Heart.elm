module Heart where


type alias Model =
  { heartState : HeartState
  , leftActions : List Action
  , nextAction : Action
  }


type alias HeartState =
  { left : PartState
  , right : PartState
  , upperLeft : PartState
  , upperRight : PartState
  , bottomLeft : PartState
  , bottomRight : PartState
  , centerLeft : PartState
  , centerRight : PartState
  }


type Part = Left | Right | UpperLeft | UpperRight | BottomLeft | BottomRight | CenterLeft | CenterRight


type PartState = Off | Ready | On


type Action = Reset | Trace Part | Complete


init : Model
init =
  { heartState = turnedOffHeartState
  , leftActions = actionStack
  , nextAction = Reset
  }


turnedOffHeartState : HeartState
turnedOffHeartState =
  { left = Off
  , right = Off
  , upperLeft = Off
  , upperRight = Off
  , bottomLeft = Off
  , bottomRight = Off
  , centerLeft = Off
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
  ( join [Reset] <| List.map ( List.map Trace ) [cL, cO, cV, cE] ) ++ [Complete]


join : List a -> List (List a) -> List a
join x = List.intersperse x >> List.concat
