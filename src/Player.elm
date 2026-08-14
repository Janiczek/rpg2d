module Player exposing (Player)

import Direction exposing (Direction)


type alias Player =
    { x : Int
    , y : Int
    , direction : Direction
    , lastMoveTimeMs : Float
    }
