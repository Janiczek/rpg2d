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

import List.Cartesian
import Math.Matrix4 exposing (Mat4)
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


staticTilesMesh : Renderer.Mesh
staticTilesMesh =
    (layer0Grass ++ layer1Walls)
        |> Renderer.tilesToMesh


webGLEntities : Renderer.Mesh -> Tileset.LoadedTileset -> Mat4 -> Mat4 -> List WebGL.Entity
webGLEntities playerMesh_ tileset cameraProjection cameraTranslateMatrix =
    [ staticTilesMesh
    , playerMesh_
    ]
        |> List.map (Renderer.meshToWebGLEntity tileset cameraProjection cameraTranslateMatrix)


hasSolid : Int -> Int -> Bool
hasSolid x y =
    -- for now, only about walls
    Set.member ( x, y ) wallCoords
