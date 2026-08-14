module Types exposing
    ( BackendModel
    , BackendMsg(..)
    , FrontendModel
    , FrontendMsg(..)
    , ToBackend(..)
    , ToFrontend(..)
    )

import Camera exposing (Camera)
import Keyboard
import Player exposing (Player)
import Tileset exposing (Tileset)
import WebGL.Texture exposing (Texture)


type alias FrontendModel =
    { tileset : Maybe Texture
    , keys : List Keyboard.Key
    , timeMs : Float
    , player : Player
    , camera : Camera
    }


type alias BackendModel =
    {}


type FrontendMsg
    = DontCareAboutUrl
    | LoadedTileset Tileset
    | GotKeys Keyboard.Msg
    | Tick Float


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
