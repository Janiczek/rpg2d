module Player exposing
    ( Movement
    , Player
    , canDoRepeatedMovement
    , disableRepeatedMovementThrottle
    , movementSpeedMsPerTile
    )

import Direction exposing (Direction)


type alias Player =
    -- logical x,y position - during movement, the final one
    { x : Int
    , y : Int
    , direction : Direction
    , -- >0 = we're currently holding something after we've moved:
      -- waiting for another repeated movement "tick"
      lastMoveTimeMs : Float
    , movement : Maybe Movement
    }


type alias Movement =
    { origX : Float
    , origY : Float
    , moveStartMs : Float
    }


disableRepeatedMovementThrottle : Player -> Player
disableRepeatedMovementThrottle p =
    { p | lastMoveTimeMs = 0 }


movementSpeedMsPerTile : Float
movementSpeedMsPerTile =
    150


canDoRepeatedMovement : Player -> Bool
canDoRepeatedMovement player =
    player.movement == Nothing
