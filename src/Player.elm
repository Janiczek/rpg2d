module Player exposing
    ( Movement
    , Player
    , canDoRepeatedMovement
    , disableRepeatedMovementThrottle
    , lerpPosition
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


canDoRepeatedMovement : Float -> Player -> Bool
canDoRepeatedMovement timeMs player =
    (player.movement == Nothing)
        && (timeMs - player.lastMoveTimeMs >= movementSpeedMsPerTile)


{-| Assumes a clamped percentage 0..1
-}
lerp : Float -> Float -> Float -> Float
lerp pct a b =
    a + (b - a) * pct


{-| Position for movement between tiles
-}
lerpPosition : Float -> Player -> ( Float, Float )
lerpPosition timeMs player =
    case player.movement of
        Nothing ->
            ( toFloat player.x, toFloat player.y )

        Just mvmt ->
            -- TODO we could be more performant if we looked at Direction and only lerped one of the numbers
            let
                -- How far along the movement we are
                percentage =
                    Basics.clamp 0 1 <| (timeMs - mvmt.moveStartMs) / movementSpeedMsPerTile
            in
            ( lerp percentage mvmt.origX (toFloat player.x)
            , lerp percentage mvmt.origY (toFloat player.y)
            )
