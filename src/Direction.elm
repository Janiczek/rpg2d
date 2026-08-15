module Direction exposing
    ( Direction(..)
    , delta
    , rotation
    )

import Math.Matrix4 exposing (Mat4)
import Renderer


type Direction
    = Left
    | Up
    | Right
    | Down


delta : Direction -> ( Int, Int )
delta dir =
    case dir of
        Down ->
            ( 0, 1 )

        Left ->
            ( -1, 0 )

        Right ->
            ( 1, 0 )

        Up ->
            ( 0, -1 )


{-| The player sprite is initially looking down
-}
rotation : Direction -> (Mat4 -> Mat4)
rotation dir =
    case dir of
        Down ->
            Renderer.noRotation

        Left ->
            Renderer.rotateClockwise

        Right ->
            Renderer.rotateAnticlockwise

        Up ->
            Renderer.rotate180
