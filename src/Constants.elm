module Constants exposing
    ( cameraEasing
    , playerEasing
    )

import Ease exposing (Easing)


playerEasing : Easing
playerEasing =
    Ease.inOutSine


cameraEasing : Easing
cameraEasing =
    playerEasing
