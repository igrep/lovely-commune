module Heart.Part where


type alias Model =
  { id      : String
  , filled  : Percentage
  , readyTo : Direction
  }


type Direction = FromTopLeft | FromBottomRight | TurnOff


type Action = MoveOver Int Int | ResetAction


type alias Percentage = Float
