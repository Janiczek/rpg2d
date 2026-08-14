port module Sound exposing (Sound(..), play)


port playSound : String -> Cmd msg


type Sound
    = Bounce


name : Sound -> String
name sound =
    case sound of
        Bounce ->
            "bounce"


play : Sound -> Cmd msg
play sound =
    playSound (name sound)
