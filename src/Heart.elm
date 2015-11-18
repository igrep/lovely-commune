module Heart where

type alias Heart =
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
  Reset :: ( join Reset <| List.map ( List.map Trace ) [cL, cO, cV, cE] ) ++ [Complete]


join : a -> List (List a) -> List a
join x = List.intersperse [x] >> List.concat
