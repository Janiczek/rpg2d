module Renderer exposing (Tile, tile, tileToWebGLEntity)

import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2)
import Tileset
import WebGL exposing (Entity, Mesh, Shader)
import WebGL.Settings exposing (Setting)
import WebGL.Settings.Blend as Blend
import WebGL.Texture exposing (Texture)


type alias Tile =
    { tileCoord : Vec2
    , sceneX : Int
    , sceneY : Int
    }


{-| Allow for seeing stuff underneath the transparent pixels of the current WebGL entity:

final = (source × sourceAlpha) + (destination × (1 - sourceAlpha))

-}
blend : List Setting
blend =
    [ Blend.add Blend.srcAlpha Blend.oneMinusSrcAlpha ]


tileToWebGLEntity : Tileset.LoadedTileset -> Mat4 -> Mat4 -> Tile -> Entity
tileToWebGLEntity tileset cameraProjection cameraTranslateMatrix t =
    WebGL.entityWith
        blend
        vertexShader
        fragmentShader
        quadMesh
        { projection = cameraProjection
        , view = cameraTranslateMatrix
        , model = Mat4.makeTranslate3 (toFloat t.sceneX) (toFloat t.sceneY) 0
        , texture = tileset
        , tileCoord = t.tileCoord
        , tileSizeUV = Tileset.tileSizeUV
        }


tile : Int -> Int -> Vec2 -> Tile
tile sceneX sceneY tileCoord =
    { tileCoord = tileCoord
    , sceneX = sceneX
    , sceneY = sceneY
    }



-- WEBGL STUFF


type alias Vertex =
    { position : Vec2 }


{-| Square! Two triangles togetner.

    0,1    1,1
    -------
    |    /|
    |   / |
    |  /  |
    | /   |
    |/    |
    -------
    0,0    1,0

Note: I might be wrong about where each axis' 0 and 1 are.

For us, each tile is a square shape.

-}
quadMesh : Mesh Vertex
quadMesh =
    let
        p00 =
            Vertex (Vec2.vec2 0 0)

        p10 =
            Vertex (Vec2.vec2 1 0)

        p11 =
            Vertex (Vec2.vec2 1 1)

        p01 =
            Vertex (Vec2.vec2 0 1)
    in
    WebGL.triangles
        [ ( p00, p10, p11 )
        , ( p00, p11, p01 )
        ]


type alias Uniforms =
    { projection : Mat4
    , view : Mat4
    , model : Mat4
    , texture : Texture
    , tileCoord : Vec2
    , tileSizeUV : Vec2
    }


type alias Varyings =
    { vTexCoord : Vec2 }


{-| Runs once per vertex. Everything used here:

  - position: original Mesh vertices (0..1 x 0..1)
  - projection: maps the world (Map.\*, eg. of size 8x5) to the canvas
  - model: translates the tile inside the world (eg. onto position (3,2))
  - tileCoord: selects which tile from the tileset texture to sample
  - tileSizeUV: one tile's size as fraction of the whole tileset texture

The actual sampling (colors used for pixels, reading the texture pixels) is done
in the fragmentShader below.

-}
vertexShader : Shader Vertex Uniforms Varyings
vertexShader =
    [glsl|
        attribute vec2 position;

        uniform mat4 projection;
        uniform mat4 view;
        uniform mat4 model;
        uniform vec2 tileCoord;
        uniform vec2 tileSizeUV;

        varying vec2 vTexCoord;

        void main () {
            vTexCoord = vec2(
                (tileCoord.x + position.x) * tileSizeUV.x,
                (tileCoord.y + position.y) * tileSizeUV.y
            );
            gl_Position = projection * view * model * vec4(position, 0.0, 1.0);
        }
    |]


{-| Reads the texture for each relevant pixel after the vertexShader selected
which part of the texture to look at.
-}
fragmentShader : Shader {} Uniforms Varyings
fragmentShader =
    [glsl|
        precision mediump float;

        uniform sampler2D texture;
        varying vec2 vTexCoord;

        void main () {
            gl_FragColor = texture2D(texture, vTexCoord);
        }
    |]
