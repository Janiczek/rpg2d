module Map exposing
    ( widthTiles, heightTiles
    , initPlayerX, initPlayerY
    , webGLEntities, hasSolid
    )

{-|

@docs widthTiles, heightTiles
@docs initPlayerX, initPlayerY
@docs webGLEntities, hasSolid

-}

import Constants
import Direction
import List.Cartesian
import Math.Matrix4 as Mat4 exposing (Mat4)
import Player exposing (Player)
import Renderer
import Set exposing (Set)
import Tileset exposing (TileType(..))
import WebGL


widthTiles : number
widthTiles =
    15


heightTiles : number
heightTiles =
    11


lastColumn : number
lastColumn =
    widthTiles - 1


lastRow : number
lastRow =
    heightTiles - 1


initPlayerX : number
initPlayerX =
    3


initPlayerY : number
initPlayerY =
    2



-- Map


allXs : List Int
allXs =
    List.range 0 lastColumn


allYs : List Int
allYs =
    List.range 0 lastRow


grass1Tile : ( number, number )
grass1Tile =
    Tileset.coord T_Grass1


grass2Tile : ( number, number )
grass2Tile =
    Tileset.coord T_Grass2


grass3Tile : ( number, number )
grass3Tile =
    Tileset.coord T_Grass3


grass4Tile : ( number, number )
grass4Tile =
    Tileset.coord T_Grass4


playerTile : ( number, number )
playerTile =
    Tileset.coord T_Player


wallTile : ( number, number )
wallTile =
    Tileset.coord T_Wall


coordsToIndex : number -> number -> number
coordsToIndex x y =
    y * widthTiles + x


layer0Grass : List Renderer.Tile
layer0Grass =
    List.Cartesian.map2
        (\x y ->
            let
                i =
                    coordsToIndex x y |> modBy 4

                tile =
                    case i of
                        0 ->
                            grass1Tile

                        1 ->
                            grass2Tile

                        2 ->
                            grass3Tile

                        3 ->
                            grass4Tile

                        _ ->
                            grass1Tile
            in
            Renderer.tile (toFloat x) (toFloat y) tile Renderer.noRotation
        )
        allXs
        allYs


wallCoords : Set ( Int, Int )
wallCoords =
    [ allXs |> List.concatMap (\x -> [ ( x, 0 ), ( x, lastRow ) ])
    , allYs |> List.concatMap (\y -> [ ( 0, y ), ( lastColumn, y ) ])
    ]
        |> List.concat
        |> Set.fromList


layer1Walls : List Renderer.Tile
layer1Walls =
    wallCoords
        |> Set.toList
        |> List.map (\( x, y ) -> Renderer.tile (toFloat x) (toFloat y) wallTile Renderer.noRotation)


{-| Everything except for the player.
-}
mapMesh : { camera | x : Float, y : Float } -> Float -> Float -> Renderer.Mesh
mapMesh camera cameraWidth cameraHeight =
    -- TODO try and measure without the filtering
    let
        cameraLeft : Float
        cameraLeft =
            camera.x

        cameraRight : Float
        cameraRight =
            cameraLeft + cameraWidth - 1

        cameraTop : Float
        cameraTop =
            camera.y

        cameraBottom : Float
        cameraBottom =
            cameraTop + cameraHeight - 1
    in
    (layer0Grass ++ layer1Walls)
        -- Later with dynamic maps we'd want the actual List.range generation to be
        -- constrained, not List.filter after the fact.
        |> List.filter (isVisible cameraLeft cameraRight cameraTop cameraBottom)
        |> Renderer.tilesToMesh


playerMesh : Float -> Player -> Renderer.Mesh
playerMesh timeMs p =
    let
        ( px, py ) =
            Player.position Constants.playerEasing timeMs p
    in
    Renderer.tilesToMesh
        [ Renderer.tile px py playerTile (Direction.rotation p.direction) ]


webGLEntities : Float -> Player -> { camera | x : Float, y : Float } -> Float -> Float -> Tileset.LoadedTileset -> Mat4 -> Mat4 -> List WebGL.Entity
webGLEntities timeMs player camera cameraWidth cameraHeight tileset cameraProjection cameraTranslateMatrix =
    [ mapMesh camera cameraWidth cameraHeight
        |> Renderer.meshToWebGLEntity tileset cameraProjection cameraTranslateMatrix Mat4.identity
    , playerMesh timeMs player
        |> Renderer.meshToWebGLEntity tileset cameraProjection cameraTranslateMatrix Mat4.identity
    ]


{-| We'll add 1 tile safety margin for cases where the player is moving and
would see a new tile gradually appear.
-}
isVisible : Float -> Float -> Float -> Float -> Renderer.Tile -> Bool
isVisible cameraLeft cameraRight cameraTop cameraBottom tile =
    (tile.sceneX >= cameraLeft - 1)
        && (tile.sceneX <= cameraRight + 1)
        && (tile.sceneY >= cameraTop - 1)
        && (tile.sceneY <= cameraBottom + 1)


hasSolid : Int -> Int -> Bool
hasSolid x y =
    -- for now, only about walls
    Set.member ( x, y ) wallCoords
