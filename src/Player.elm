module Player exposing
    ( Player, Movement
    , movementSpeedMsPerTile
    , canDoRepeatedMovement, disableRepeatedMovementThrottle
    , position
    , recomputeMesh
    )

{-|

@docs Player, Movement
@docs movementSpeedMsPerTile
@docs canDoRepeatedMovement, disableRepeatedMovementThrottle
@docs position
@docs recomputeMesh

-}

import Constants
import Direction exposing (Direction)
import Ease exposing (Easing)
import Lerp
import Renderer
import Tileset


type alias Player =
    -- logical x,y position - during movement, the final one
    { x : Int
    , y : Int
    , direction : Direction
    , -- >0 = we're currently holding something after we've moved:
      -- waiting for another repeated movement "tick"
      lastMoveTimeMs : Float
    , movement : Maybe Movement
    , mesh : Renderer.Mesh
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


position : Easing -> Float -> Player -> ( Float, Float )
position easing timeMs player =
    case player.movement of
        Nothing ->
            ( toFloat player.x
            , toFloat player.y
            )

        Just mvmt ->
            -- TODO we could be more performant if we looked at Direction and only lerped one of the numbers
            let
                pct =
                    Lerp.percentage mvmt.moveStartMs timeMs movementSpeedMsPerTile
            in
            ( Lerp.roundToPixels Tileset.tileWidthPx <| Lerp.lerp easing pct mvmt.origX (toFloat player.x)
            , Lerp.roundToPixels Tileset.tileHeightPx <| Lerp.lerp easing pct mvmt.origY (toFloat player.y)
            )


playerTile : ( number, number )
playerTile =
    Tileset.coord Tileset.T_Player


recomputeMesh : Float -> Player -> Player
recomputeMesh timeMs p =
    let
        ( px, py ) =
            position Constants.playerEasing timeMs p
    in
    { p
        | mesh =
            Renderer.tilesToMesh
                [ Renderer.tile px py playerTile (Direction.rotation p.direction) ]
    }
