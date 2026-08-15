module Frontend exposing (Model, app)

import Browser
import Browser.Events
import Browser.Navigation as Nav
import Camera
import Constants
import Direction exposing (Direction(..))
import Html exposing (Html)
import Html.Attributes
import Keyboard
import Lamdera
import Map
import Player exposing (Player)
import Renderer
import Sound exposing (Sound)
import Tileset
import Types exposing (FrontendModel, FrontendMsg(..), ToFrontend(..))
import Url
import WebGL


unzoomedCanvasWidthPx : number
unzoomedCanvasWidthPx =
    Camera.widthTiles * Tileset.tileWidthPx


unzoomedCanvasHeightPx : number
unzoomedCanvasHeightPx =
    Camera.heightTiles * Tileset.tileHeightPx


zoomedCanvasWidthPx : number
zoomedCanvasWidthPx =
    Constants.pxZoom * unzoomedCanvasWidthPx


zoomedCanvasHeightPx : number
zoomedCanvasHeightPx =
    Constants.pxZoom * unzoomedCanvasHeightPx


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
subscriptions _ =
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
            , movement = Nothing
            }
    in
    ( { tileset = Nothing
      , arrowKeys = []
      , timeMs = 0
      , player = player
      , camera =
            Camera.centeredAt player 0
                |> Camera.clamp
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
                    Keyboard.update keyMsg model.arrowKeys

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
            ( { model
                | arrowKeys = arrowKeys
                , player =
                    if
                        List.isEmpty arrowKeys
                            && not (List.isEmpty model.arrowKeys)
                    then
                        -- stopped holding keys, let's disable the repeated movement throttle
                        model.player |> Player.disableRepeatedMovementThrottle

                    else
                        model.player
              }
            , Cmd.none
            )

        Tick dt ->
            let
                newTimeMs =
                    model.timeMs + dt

                ( newPlayer, soundToPlay ) =
                    tickPlayer newTimeMs model.arrowKeys model.player
            in
            ( { model
                | timeMs = newTimeMs
                , player = newPlayer
                , camera =
                    Camera.centeredAt newPlayer newTimeMs
                        |> Camera.clamp
              }
            , case soundToPlay of
                Nothing ->
                    Cmd.none

                Just sound ->
                    Sound.play sound
            )


tickPlayer : Float -> List Keyboard.Key -> Player -> ( Player, Maybe Sound )
tickPlayer timeMs arrowKeys player =
    player
        |> endMovement timeMs
        |> walk arrowKeys timeMs


endMovement : Float -> Player -> Player
endMovement timeMs player =
    case player.movement of
        Nothing ->
            player

        Just movement ->
            if (timeMs - movement.moveStartMs) >= Player.movementSpeedMsPerTile then
                { player | movement = Nothing }

            else
                player


walk : List Keyboard.Key -> Float -> Player -> ( Player, Maybe Sound )
walk arrowKeys timeMs player =
    case arrowKeys of
        [] ->
            ( player, Nothing )

        lastArrowKey :: _ ->
            if Player.canDoRepeatedMovement timeMs player then
                let
                    goWith dir =
                        let
                            ( dx, dy ) =
                                Direction.delta dir

                            targetX =
                                player.x + dx

                            targetY =
                                player.y + dy
                        in
                        if Map.hasSolid targetX targetY then
                            -- Can't move there: collision!
                            ( { player
                                | lastMoveTimeMs = timeMs
                                , direction = dir
                              }
                            , Just Sound.Bounce
                            )

                        else
                            ( { player
                                | x = targetX
                                , y = targetY
                                , direction = dir
                                , lastMoveTimeMs = timeMs
                                , movement =
                                    Just
                                        { origX = toFloat player.x
                                        , origY = toFloat player.y
                                        , moveStartMs = timeMs
                                        }
                              }
                            , Nothing
                            )
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
                        ( player, Nothing )

            else
                -- Ignore, too soon after previous movement.
                ( player, Nothing )


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
                model.timeMs
                model.camera
                Camera.widthTiles
                Camera.heightTiles
                |> List.length
    in
    Html.ul []
        [ Html.li [] [ Html.text <| "arrow keys: " ++ Debug.toString model.arrowKeys ]
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
                model.timeMs
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
