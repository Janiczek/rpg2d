module Player exposing (Player)


type alias Player =
    { x : Int
    , y : Int
    , lastMoveTimeMs : Float
    }
