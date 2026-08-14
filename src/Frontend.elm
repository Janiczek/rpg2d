module Frontend exposing (Model, app)

import Browser
import Browser.Events
import Browser.Navigation as Nav
import Camera
import Direction exposing (Direction(..))
import Html exposing (Html)
import Html.Attributes
import Keyboard
import Lamdera
import Map
import Player exposing (Player)
import Renderer
import Tileset
import Types exposing (FrontendModel, FrontendMsg(..), ToFrontend(..))
import Url
import WebGL


pxZoom : number
pxZoom =
    2


unzoomedCanvasWidthPx : number
unzoomedCanvasWidthPx =
    Camera.widthTiles * Tileset.tileWidthPx


unzoomedCanvasHeightPx : number
unzoomedCanvasHeightPx =
    Camera.heightTiles * Tileset.tileHeightPx


zoomedCanvasWidthPx : number
zoomedCanvasWidthPx =
    pxZoom * unzoomedCanvasWidthPx


zoomedCanvasHeightPx : number
zoomedCanvasHeightPx =
    pxZoom * unzoomedCanvasHeightPx


type alias Model =
    FrontendModel


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = \_ -> DontCareAboutUrl
        , onUrlChange = \_ -> DontCareAboutUrl
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = subscriptions
        , view = view
        }


subscriptions : Model -> Sub FrontendMsg
subscriptions model =
    Sub.batch
        [ Keyboard.subscriptions |> Sub.map GotKeys
        , Browser.Events.onAnimationFrameDelta Tick
        ]


init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init _ _ =
    let
        player : Player
        player =
            { x = Map.initPlayerX
            , y = Map.initPlayerY
            , direction = Down
            , lastMoveTimeMs = 0
            }
    in
    ( { tileset = Nothing
      , keys = []
      , timeMs = 0
      , player = player
      , camera = Camera.centeredAt player |> Camera.clamp
      }
    , Tileset.load LoadedTileset
    )


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        DontCareAboutUrl ->
            ( model, Cmd.none )

        LoadedTileset tileset ->
            ( { model | tileset = tileset }
            , Cmd.none
            )

        GotKeys keyMsg ->
            let
                keys =
                    Keyboard.update keyMsg model.keys
            in
            ( { model | keys = keys }
            , Cmd.none
            )

        Tick dt ->
            let
                newTimeMs =
                    model.timeMs + dt

                newPlayer =
                    tickPlayer newTimeMs model.keys model.player
            in
            ( { model
                | timeMs = newTimeMs
                , player = newPlayer
                , camera = Camera.centeredAt newPlayer |> Camera.clamp
              }
            , Cmd.none
            )


tickPlayer : Float -> List Keyboard.Key -> Player -> Player
tickPlayer timeMs keys player =
    let
        arrowKeys =
            keys
                |> List.filter
                    (\key ->
                        (key == Keyboard.ArrowLeft)
                            || (key == Keyboard.ArrowRight)
                            || (key == Keyboard.ArrowUp)
                            || (key == Keyboard.ArrowDown)
                    )
    in
    player
        |> walk arrowKeys timeMs


{-| The player will move in discrete jumps, 1 jump per this many ms
-}
walkSpeedMsPerTile : Float
walkSpeedMsPerTile =
    250


isThrottlingRepeatedMovement : Float -> Float -> Bool
isThrottlingRepeatedMovement timeMs playerLastMoveTimeMs =
    timeMs - playerLastMoveTimeMs < walkSpeedMsPerTile


walk : List Keyboard.Key -> Float -> Player -> Player
walk arrowKeys timeMs player =
    if isThrottlingRepeatedMovement timeMs player.lastMoveTimeMs then
        -- Ignore, too soon after previous movement.
        player

    else
        case arrowKeys of
            [] ->
                player

            lastArrowKey :: _ ->
                let
                    goWith dir =
                        let
                            ( dx, dy ) =
                                Direction.delta dir

                            newX =
                                player.x + dx

                            newY =
                                player.y + dy
                        in
                        if Map.hasSolid newX newY then
                            -- TODO play bump sound?
                            player

                        else
                            { x = newX
                            , y = newY
                            , direction = dir
                            , lastMoveTimeMs = timeMs
                            }
                in
                case lastArrowKey of
                    Keyboard.ArrowLeft ->
                        goWith Left

                    Keyboard.ArrowRight ->
                        goWith Right

                    Keyboard.ArrowUp ->
                        goWith Up

                    Keyboard.ArrowDown ->
                        goWith Down

                    _ ->
                        player


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )


view : Model -> Browser.Document FrontendMsg
view model =
    { title = "2D RPG"
    , body =
        [ viewDebug model
        , viewGame model
        ]
    }


viewDebug : Model -> Html msg
viewDebug model =
    let
        tilesSent : Int
        tilesSent =
            Map.tiles
                model.player
                model.camera
                Camera.widthTiles
                Camera.heightTiles
                |> List.length
    in
    Html.ul []
        [ Html.li [] [ Html.text <| "keys: " ++ Debug.toString model.keys ]
        , Html.li [] [ Html.text <| "player: " ++ Debug.toString model.player ]
        , Html.li [] [ Html.text <| "camera: " ++ Debug.toString model.camera ]
        , Html.li [] [ Html.text <| "tiles sent to GPU: " ++ String.fromInt tilesSent ]
        ]


viewGame : Model -> Html msg
viewGame model =
    case model.tileset of
        Nothing ->
            Html.text "Loading tileset..."

        Just tileset ->
            Map.tiles
                model.player
                model.camera
                Camera.widthTiles
                Camera.heightTiles
                |> List.map
                    (Renderer.tileToWebGLEntity
                        tileset
                        Camera.projection
                        (Camera.translateMatrix model.camera)
                    )
                |> WebGL.toHtml
                    [ Html.Attributes.width zoomedCanvasWidthPx
                    , Html.Attributes.height zoomedCanvasHeightPx
                    ]
