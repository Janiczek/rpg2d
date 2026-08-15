module Renderer exposing
    ( Tile, tile
    , Mesh, Vertex, tilesToMesh, meshToWebGLEntity
    , noRotation, rotateClockwise, rotate180, rotateAnticlockwise
    )

{-|


## Renderables

@docs Tile, tile
@docs Mesh, Vertex, tilesToMesh, meshToWebGLEntity


## Rotations

@docs noRotation, rotateClockwise, rotate180, rotateAnticlockwise

-}

import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2)
import Math.Vector3 as Vec3
import Tileset
import WebGL exposing (Entity, Shader)
import WebGL.Settings exposing (Setting)
import WebGL.Settings.Blend as Blend
import WebGL.Texture exposing (Texture)


type alias Tile =
    { rotation : Mat4 -> Mat4
    , sceneX : Float
    , sceneY : Float
    , tilesetX : Float
    , tilesetY : Float
    }


{-| Allow for seeing stuff underneath the transparent pixels of the current WebGL entity:

final = (source × sourceAlpha) + (destination × (1 - sourceAlpha))

-}
blend : List Setting
blend =
    [ Blend.add Blend.srcAlpha Blend.oneMinusSrcAlpha ]


meshToWebGLEntity : Tileset.LoadedTileset -> Mat4 -> Mat4 -> Mesh -> Entity
meshToWebGLEntity tileset cameraProjection cameraTranslateMatrix mesh =
    WebGL.entityWith
        blend
        vertexShader
        fragmentShader
        mesh
        { projection = cameraProjection
        , view = cameraTranslateMatrix
        , texture = tileset
        }


tile : Float -> Float -> ( Float, Float ) -> (Mat4 -> Mat4) -> Tile
tile sceneX sceneY ( tilesetX, tilesetY ) rotation =
    { rotation = rotation
    , sceneX = sceneX
    , sceneY = sceneY
    , tilesetX = tilesetX
    , tilesetY = tilesetY
    }



-- Rotation


noRotation : Mat4 -> Mat4
noRotation mat =
    mat


aboutCenter : (Mat4 -> Mat4) -> Mat4 -> Mat4
aboutCenter rotationAboutOrigin original =
    original
        |> Mat4.translate3 0.5 0.5 0
        |> rotationAboutOrigin
        |> Mat4.translate3 -0.5 -0.5 0


rotateClockwise : Mat4 -> Mat4
rotateClockwise =
    aboutCenter (Mat4.rotate (pi / 2) Vec3.k)


rotateAnticlockwise : Mat4 -> Mat4
rotateAnticlockwise =
    aboutCenter (Mat4.rotate (-pi / 2) Vec3.k)


rotate180 : Mat4 -> Mat4
rotate180 =
    aboutCenter (Mat4.rotate pi Vec3.k)



-- WEBGL STUFF


type alias Vertex =
    { worldPosition : Vec2 -- tile space (0..1 is one tile, 0..5 is 5 tiles, etc.)
    , texCoord : Vec2 -- which tile do we want to render?
    }


type alias Mesh =
    WebGL.Mesh Vertex


tileVertices : Tile -> List Vertex
tileVertices tile_ =
    let
        uvW =
            Tileset.tileWidthPx / Tileset.widthPx

        uvH =
            Tileset.tileHeightPx / Tileset.heightPx

        localTransform : Mat4
        localTransform =
            tile_.rotation Mat4.identity

        corner x y =
            let
                rotated =
                    Mat4.transform localTransform (Vec3.vec3 x y 0)

                worldX =
                    tile_.sceneX + Vec3.getX rotated

                worldY =
                    tile_.sceneY + Vec3.getY rotated
            in
            { worldPosition =
                Vec2.vec2
                    worldX
                    worldY
            , texCoord =
                Vec2.vec2
                    ((tile_.tilesetX + x) * uvW)
                    ((tile_.tilesetY + y) * uvH)
            }
    in
    [ corner 0 0
    , corner 1 0
    , corner 1 1
    , corner 0 1
    ]


tilesToMesh : List Tile -> Mesh
tilesToMesh tiles =
    let
        step tile_ ( vs, is, base ) =
            -- TODO PERF: concat once? is order important?
            ( vs ++ tileVertices tile_
            , is
                ++ [ ( base
                     , base + 1
                     , base + 2
                     )
                   , ( base
                     , base + 2
                     , base + 3
                     )
                   ]
            , base + 4
            )

        ( vertices, indices, _ ) =
            List.foldl step ( [], [], 0 ) tiles
    in
    WebGL.indexedTriangles vertices indices


type alias Uniforms =
    { projection : Mat4
    , view : Mat4
    , texture : Texture
    }


type alias Varyings =
    { vTexCoord : Vec2 }


{-| Runs once per vertex. Everything used here:

  - position: original Mesh vertices (0..1 x 0..1)
  - projection: maps the world (Map.\*, eg. of size 8x5) to the canvas

The actual sampling (colors used for pixels, reading the texture pixels) is done
in the fragmentShader below.

-}
vertexShader : Shader Vertex Uniforms Varyings
vertexShader =
    [glsl|
        attribute vec2 worldPosition;
        attribute vec2 texCoord;

        uniform mat4 projection;
        uniform mat4 view;

        varying vec2 vTexCoord;

        void main () {
            vTexCoord = texCoord;
            gl_Position = projection * view * vec4(worldPosition, 0.0, 1.0);
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
