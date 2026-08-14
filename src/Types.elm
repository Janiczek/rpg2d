module Types exposing
    ( BackendModel
    , BackendMsg(..)
    , FrontendModel
    , FrontendMsg(..)
    , ToBackend(..)
    , ToFrontend(..)
    )

import Browser exposing (UrlRequest)
import Browser.Navigation
import Keyboard
import Player exposing (Player)
import Tileset exposing (Tileset)
import Url exposing (Url)
import WebGL.Texture exposing (Texture)


type alias FrontendModel =
    { tileset : Maybe Texture
    , keys : List Keyboard.Key
    , time : Float
    , player : Player
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
