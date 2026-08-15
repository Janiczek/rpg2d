module Player exposing
    ( Player, Movement
    , movementSpeedMsPerTile
    , position
    , recomputeMesh
    )

{-|

@docs Player, Movement
@docs movementSpeedMsPerTile
@docs position
@docs recomputeMesh

-}

import Constants
import Direction exposing (Direction)
import Ease exposing (Easing)
import Lerp
import List.Extra
import Renderer
import Tileset


type alias Player =
    -- logical x,y position - during movement, the final one
    { x : Int
    , y : Int
    , direction : Direction
    , movement : Maybe Movement
    , mesh : Renderer.Mesh
    }


type alias Movement =
    { origX : Float
    , origY : Float
    , -- Useful for movement speed and throttling
      currentTileMoveStartMs : Float
    , -- Useful for animations
      movementChainStartMs : Float
    }


movementSpeedMsPerTile : Float
movementSpeedMsPerTile =
    150


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
                    Lerp.percentage mvmt.currentTileMoveStartMs timeMs movementSpeedMsPerTile
            in
            ( Lerp.roundToPixels Tileset.tileWidthPx <| Lerp.lerp easing pct mvmt.origX (toFloat player.x)
            , Lerp.roundToPixels Tileset.tileHeightPx <| Lerp.lerp easing pct mvmt.origY (toFloat player.y)
            )


playerTileStanding : ( number, number )
playerTileStanding =
    Tileset.coord Tileset.T_Player1


playerTileMoving : List ( number, number )
playerTileMoving =
    [ Tileset.coord Tileset.T_Player1
    , Tileset.coord Tileset.T_Player3
    ]


playerTileMovingLength : Int
playerTileMovingLength =
    List.length playerTileMoving


{-| 1 tile change per ... ms
-}
playerAnimationSpeedMs : number
playerAnimationSpeedMs =
    150


recomputeMesh : Float -> Player -> Player
recomputeMesh timeMs p =
    let
        ( px, py ) =
            position Constants.playerEasing timeMs p

        tile =
            case p.movement of
                Nothing ->
                    playerTileStanding

                Just movement ->
                    let
                        index =
                            ((timeMs - movement.movementChainStartMs) / playerAnimationSpeedMs)
                                |> truncate
                                |> (+) 1
                                |> modBy playerTileMovingLength
                    in
                    List.Extra.getAt index playerTileMoving
                        |> Maybe.withDefault playerTileStanding
    in
    { p
        | mesh =
            Renderer.tilesToMesh
                [ Renderer.tile px py tile (Direction.rotation p.direction) ]
    }
