module Map exposing
    ( hasSolid
    , heightTiles
    , initPlayerX
    , initPlayerY
    , tiles
    , widthTiles
    )

import List.Cartesian
import Math.Vector2 exposing (Vec2)
import Renderer
import Set exposing (Set)
import Tileset exposing (TileType(..))


widthTiles : number
widthTiles =
    13


heightTiles : number
heightTiles =
    9


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


grassTile : Vec2
grassTile =
    Tileset.tileCoord T_Grass


playerTile : Vec2
playerTile =
    Tileset.tileCoord T_Player


wallTile : Vec2
wallTile =
    Tileset.tileCoord T_Wall


layer0Grass : List Renderer.Tile
layer0Grass =
    List.Cartesian.map2
        (\x y -> Renderer.tile x y grassTile)
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
        |> List.map (\( x, y ) -> Renderer.tile x y wallTile)


tiles : { player | x : Int, y : Int } -> { camera | x : Float, y : Float } -> Int -> Int -> List Renderer.Tile
tiles p camera cameraWidth cameraHeight =
    let
        player : Renderer.Tile
        player =
            Renderer.tile p.x p.y playerTile

        cameraLeft : Int
        cameraLeft =
            truncate camera.x

        cameraRight : Int
        cameraRight =
            cameraLeft + cameraWidth - 1

        cameraTop : Int
        cameraTop =
            truncate camera.y

        cameraBottom : Int
        cameraBottom =
            cameraTop + cameraHeight - 1
    in
    -- Later with dynamic maps we'd want the actual List.range generation to be
    -- constrained, not List.filter after the fact.
    (layer0Grass ++ (player :: layer1Walls))
        |> List.filter (isVisible cameraLeft cameraRight cameraTop cameraBottom)


isVisible : Int -> Int -> Int -> Int -> Renderer.Tile -> Bool
isVisible cameraLeft cameraRight cameraTop cameraBottom tile =
    (tile.sceneX >= cameraLeft)
        && (tile.sceneX <= cameraRight)
        && (tile.sceneY >= cameraTop)
        && (tile.sceneY <= cameraBottom)


hasSolid : Int -> Int -> Bool
hasSolid x y =
    -- for now, only about walls
    Set.member ( x, y ) wallCoords
