module Camera exposing
    ( Camera, centeredAt
    , clamp
    , widthTiles, heightTiles
    , projection, translateMatrix
    )

{-|


## Creation

@docs Camera, centeredAt


## Sanitization

@docs clamp


## Dimensions

@docs widthTiles, heightTiles


## WebGL

@docs projection, translateMatrix

-}

import Map
import Math.Matrix4 as Mat4 exposing (Mat4)


type alias Camera =
    { x : Float
    , y : Float
    }


widthTiles : number
widthTiles =
    9


heightTiles : number
heightTiles =
    5


halfWidth : Float
halfWidth =
    toFloat (widthTiles // 2)


halfHeight : Float
halfHeight =
    toFloat (heightTiles // 2)


{-| Makes sense for odd camera width+height.
A bit uneven (biased towards bottom right) for even width+height.
-}
centeredAt : { xy | x : Int, y : Int } -> Camera
centeredAt { x, y } =
    { x = toFloat x - halfWidth
    , y = toFloat y - halfHeight
    }


{-| Makes sure we don't show anything outside Map boundary. Instead the camera
will stop moving (and the player will stop being centered).
-}
clamp : Camera -> Camera
clamp { x, y } =
    { x = Basics.clamp 0 (Map.widthTiles - widthTiles) x
    , y = Basics.clamp 0 (Map.heightTiles - heightTiles) y
    }



-- WebGL shader stuff


{-| Camera shows only a part of the world.
-}
projection : Mat4
projection =
    Mat4.makeOrtho2D 0 widthTiles heightTiles 0


translateMatrix : Camera -> Mat4
translateMatrix { x, y } =
    Mat4.makeTranslate3 -x -y 0
