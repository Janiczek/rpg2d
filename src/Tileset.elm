module Tileset exposing
    ( LoadedTileset
    , TileType(..)
    , Tileset
    , coord
    , heightPx
    , load
    , tileHeightPx
    , tileWidthPx
    , widthPx
    )

import Task
import WebGL.Texture as Texture exposing (Texture)


type alias Tileset =
    Maybe Texture


type alias LoadedTileset =
    Texture


load : (Tileset -> msg) -> Cmd msg
load toMsg =
    Texture.loadWith
        { flipY = False

        -- For pixel perfect texture rendering
        , magnify = Texture.nearest
        , minify = Texture.nearest

        -- Our texture is not exactly 2^n
        , horizontalWrap = Texture.clampToEdge
        , verticalWrap = Texture.clampToEdge
        }
        url
        |> Task.attempt (Result.toMaybe >> toMsg)


url : String
url =
    "assets/textures/tileset_kenney_monochrome_rpg.png"


widthPx : number
widthPx =
    272


heightPx : number
heightPx =
    128


tileWidthPx : number
tileWidthPx =
    16


tileHeightPx : number
tileHeightPx =
    16


type TileType
    = T_Grass1
    | T_Grass2
    | T_Grass3
    | T_Grass4
    | T_Wall
    | T_Player1
    | T_Player3


{-| x,y (col,row)
Top-left = 0,0
Bottom-right = 16,7
-}
coord : TileType -> ( number, number )
coord tile =
    case tile of
        T_Grass1 ->
            ( 0, 0 )

        T_Grass2 ->
            ( 0, 1 )

        T_Grass3 ->
            ( 0, 2 )

        T_Grass4 ->
            ( 0, 3 )

        T_Wall ->
            ( 13, 0 )

        T_Player1 ->
            ( 0, 7 )

        T_Player3 ->
            ( 2, 7 )
