module Player exposing
    ( Player
    , canDoRepeatedMovement
    , disableRepeatedMovementThrottle
    )

import Direction exposing (Direction)


type alias Player =
    { x : Int
    , y : Int
    , direction : Direction
    , -- >0 = we're currently holding something after we've moved:
      -- waiting for another repeated movement "tick"
      lastMoveTimeMs : Float
    }


disableRepeatedMovementThrottle : Player -> Player
disableRepeatedMovementThrottle p =
    { p | lastMoveTimeMs = 0 }


{-| The player will move in discrete jumps, 1 jump per this many ms
-}
repeatedWalkSpeedMsPerTile : Float
repeatedWalkSpeedMsPerTile =
    150


canDoRepeatedMovement : Float -> Float -> Bool
canDoRepeatedMovement timeMs playerLastMoveTimeMs =
    timeMs - playerLastMoveTimeMs >= repeatedWalkSpeedMsPerTile
