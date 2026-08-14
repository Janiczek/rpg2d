module Camera exposing
    ( Camera
    , centeredAt
    , heightTiles
    , projection
    , translateMatrix
    , widthTiles
    )

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
Probably a bit uneven for even width+height? Haven't tried.
-}
centeredAt : { xy | x : Int, y : Int } -> Camera
centeredAt { x, y } =
    { x = toFloat x - halfWidth
    , y = toFloat y - halfHeight
    }



-- WebGL shader stuff


{-| Camera shows only a part of the world. (0,0) = top-left.
-}
projection : Mat4
projection =
    Mat4.makeOrtho2D 0 widthTiles heightTiles 0


translateMatrix : Camera -> Mat4
translateMatrix { x, y } =
    Mat4.makeTranslate3 -x -y 0
