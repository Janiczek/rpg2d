module Lerp exposing
    ( lerp
    , percentage
    , roundToPixels
    )

import Ease exposing (Easing)


lerp : Easing -> Float -> Float -> Float -> Float
lerp ease pct a b =
    linearLerp (ease pct) a b


roundToPixels : Float -> Float -> Float
roundToPixels tileSize lerpResult =
    toFloat (round (lerpResult * tileSize)) / tileSize


percentage : Float -> Float -> Float -> Float
percentage start current totalDuration =
    Basics.clamp 0 1 ((current - start) / totalDuration)


linearLerp : Float -> Float -> Float -> Float
linearLerp pct a b =
    a + (b - a) * pct
