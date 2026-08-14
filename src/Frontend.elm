module Frontend exposing (Model, app)

import Browser exposing (UrlRequest(..))
import Browser.Events
import Browser.Navigation as Nav
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
    4


unzoomedCanvasWidthPx : number
unzoomedCanvasWidthPx =
    Map.widthTiles * Tileset.tileWidthPx


unzoomedCanvasHeightPx : number
unzoomedCanvasHeightPx =
    Map.heightTiles * Tileset.tileHeightPx


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
subscriptions _ =
    Sub.batch
        [ Keyboard.subscriptions |> Sub.map GotKeys
        , Browser.Events.onAnimationFrameDelta ((\dt -> dt / 1000) >> Tick)
        ]


init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init _ _ =
    ( { tileset = Nothing
      , keys = []
      , time = 0
      , player =
            { x = Map.initPlayerX
            , y = Map.initPlayerY
            , lastMoveTime = 0
            }
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
            ( { model | keys = keys }, Cmd.none )

        Tick dt ->
            let
                newTime =
                    model.time + dt
            in
            ( { model
                | time = newTime
                , player = tickPlayer newTime model.keys model.player
              }
            , Cmd.none
            )


tickPlayer : Float -> List Keyboard.Key -> Player -> Player
tickPlayer time keys player =
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
        |> walk arrowKeys time


{-| The player will move in discrete jumps, 1 jump per this many seconds
-}
walkSpeedSecPerTile : Float
walkSpeedSecPerTile =
    0.25


walk : List Keyboard.Key -> Float -> Player -> Player
walk arrowKeys time player =
    if time - player.lastMoveTime < walkSpeedSecPerTile then
        -- Ignore, too soon after previous movement.
        player

    else
        case arrowKeys of
            [] ->
                player

            lastArrowKey :: _ ->
                let
                    goWith dx dy =
                        let
                            newX =
                                player.x + dx

                            newY =
                                player.y + dy
                        in
                        if Map.hasSolid newX newY then
                            -- TODO play bump sound?
                            player

                        else
                            { player
                                | x = newX
                                , y = newY
                                , lastMoveTime = time
                            }
                in
                case lastArrowKey of
                    Keyboard.ArrowLeft ->
                        goWith -1 0

                    Keyboard.ArrowRight ->
                        goWith 1 0

                    Keyboard.ArrowUp ->
                        goWith 0 -1

                    Keyboard.ArrowDown ->
                        goWith 0 1

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
    Html.ul []
        [ Html.li [] [ Html.text <| "keys: " ++ Debug.toString model.keys ]
        , Html.li [] [ Html.text <| "time (truncated): " ++ String.fromInt (truncate model.time) ]
        , Html.li [] [ Html.text <| "player: " ++ Debug.toString model.player ]
        ]


viewGame : Model -> Html msg
viewGame model =
    case model.tileset of
        Nothing ->
            Html.text "Loading tileset..."

        Just tileset ->
            Map.tiles model.player
                |> List.map (Renderer.tileToWebGLEntity tileset Map.projection)
                |> WebGL.toHtml
                    [ Html.Attributes.width zoomedCanvasWidthPx
                    , Html.Attributes.height zoomedCanvasHeightPx
                    ]
