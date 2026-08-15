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
linearLerp : Float -> Float -> Float -> Float
linearLerp pct a b =
    a + (b - a) * pct


{-| Assumes a clamped percentage 0..1
The decay parameter is useful around range 2..20 (2=slowest)
-}
expDecayLerp : Float -> Float -> Float -> Float -> Float
expDecayLerp decay pct a b =
    b + (a - b) * (Basics.e ^ (-decay * pct))


percentage : Float -> Float -> Float -> Float
percentage start current totalDuration =
    Basics.clamp 0 1 ((current - start) / totalDuration)


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
                pct =
                    percentage mvmt.moveStartMs timeMs movementSpeedMsPerTile
            in
            --( linearLerp pct mvmt.origX (toFloat player.x)
            --, linearLerp pct mvmt.origY (toFloat player.y)
            --)
            ( expDecayLerp 2 pct mvmt.origX (toFloat player.x)
            , expDecayLerp 2 pct mvmt.origY (toFloat player.y)
            )
