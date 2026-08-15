module Constants exposing
    ( cameraEasing
    , playerEasing
    , pxZoom
    )

import Ease exposing (Easing)


playerEasing : Easing
playerEasing =
    Ease.inOutSine


cameraEasing : Easing
cameraEasing =
    playerEasing


pxZoom : number
pxZoom =
    4
