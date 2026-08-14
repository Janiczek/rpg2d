module Tileset exposing
    ( LoadedTileset
    , TileType(..)
    , Tileset
    , load
    , tileCoord
    , tileHeightPx
    , tileSizeUV
    , tileWidthPx
    )

import Math.Vector2 as Vec2 exposing (Vec2)
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


{-| One tile's size as fraction of the whole tileset texture. We're trying to
pinpoint where on the whole texture (coords 0..1) is the tile we're interested
in.
-}
tileSizeUV : Vec2
tileSizeUV =
    Vec2.vec2
        (tileWidthPx / widthPx)
        (tileHeightPx / heightPx)


type TileType
    = T_Grass
    | T_Wall
    | T_Player


{-| x,y (col,row)
Top-left = 0,0
Bottom-right = 16,7
-}
tileCoord : TileType -> Vec2
tileCoord tile =
    case tile of
        T_Grass ->
            Vec2.vec2 0 0

        T_Wall ->
            Vec2.vec2 13 0

        T_Player ->
            Vec2.vec2 0 7
