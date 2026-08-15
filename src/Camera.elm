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
import Player exposing (Player)


type alias Camera =
    { x : Float
    , y : Float
    }


widthTiles : number
widthTiles =
    11


heightTiles : number
heightTiles =
    7


halfWidth : Float
halfWidth =
    toFloat (widthTiles // 2)


halfHeight : Float
halfHeight =
    toFloat (heightTiles // 2)


{-| Makes sense for odd camera width+height.
A bit uneven (biased towards bottom right) for even width+height.
-}
centeredAt : Player -> Float -> Camera
centeredAt p timeMs =
    let
        ( currentX, currentY ) =
            case p.movement of
                Nothing ->
                    ( toFloat p.x, toFloat p.y )

                Just mvmt ->
                    -- TODO we could be more performant if we looked at Direction and only lerped one of the numbers
                    let
                        -- How far along the movement we are
                        percentage =
                            Basics.clamp 0 1 <| (timeMs - mvmt.moveStartMs) / Player.movementSpeedMsPerTile
                    in
                    ( mvmt.origX + ((toFloat p.x - mvmt.origX) * percentage)
                    , mvmt.origY + ((toFloat p.y - mvmt.origY) * percentage)
                    )
    in
    { x = currentX - halfWidth
    , y = currentY - halfHeight
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
