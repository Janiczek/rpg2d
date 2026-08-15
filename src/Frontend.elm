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
        timeMs =
            0

        player : Player
        player =
            { x = Map.initPlayerX
            , y = Map.initPlayerY
            , direction = Down
            , movement = Nothing
            , mesh = Renderer.emptyMesh
            }
                |> Player.recomputeMesh timeMs
    in
    ( { tileset = Nothing
      , arrowKeys = []
      , timeMs = timeMs
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
            ( { model | arrowKeys = arrowKeys }
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
        |> handleMovement timeMs arrowKeys


handleMovement : Float -> List Keyboard.Key -> Player -> ( Player, Maybe Sound )
handleMovement timeMs arrowKeys player =
    case ( player.movement, arrowKeys ) of
        ( Nothing, [] ) ->
            -- not moving, not pressing any keys
            ( player, Nothing )

        ( Nothing, lastArrowKey :: _ ) ->
            -- can start moving, we've throttled
            moveOrCollide timeMs lastArrowKey Nothing player

        ( Just movement, [] ) ->
            if timeMs - movement.currentTileMoveStartMs >= Player.movementSpeedMsPerTile then
                -- should end the movement
                ( { player | movement = Nothing }
                    |> Player.recomputeMesh timeMs
                , Nothing
                )

            else
                -- still moving, shouldn't end the movement yet
                -- recompute the mesh
                ( player
                    |> Player.recomputeMesh timeMs
                , Nothing
                )

        ( Just movement, lastArrowKey :: _ ) ->
            if timeMs - movement.currentTileMoveStartMs >= Player.movementSpeedMsPerTile then
                -- current movement ended, we can chain a new one
                -- change the direction etc.
                { player | movement = Nothing }
                    |> moveOrCollide timeMs lastArrowKey (Just movement.movementChainStartMs)

            else
                -- we're holding a key in the middle of movement - ignore
                ( player
                    |> Player.recomputeMesh timeMs
                , Nothing
                )


moveOrCollide : Float -> Keyboard.Key -> Maybe Float -> Player -> ( Player, Maybe Sound )
moveOrCollide timeMs lastArrowKey movementChainStartMs player =
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
                -- We'll start off a movement to the same tile to play the animation and throttle, though.
                ( { player
                    | direction = dir
                    , movement =
                        Just
                            { origX = toFloat player.x
                            , origY = toFloat player.y
                            , currentTileMoveStartMs = timeMs
                            , movementChainStartMs =
                                movementChainStartMs
                                    |> Maybe.withDefault timeMs
                            }
                  }
                    |> Player.recomputeMesh timeMs
                , Just Sound.Bounce
                )

            else
                ( { player
                    | x = targetX
                    , y = targetY
                    , direction = dir
                    , movement =
                        Just
                            { origX = toFloat player.x
                            , origY = toFloat player.y
                            , currentTileMoveStartMs = timeMs
                            , movementChainStartMs =
                                movementChainStartMs
                                    |> Maybe.withDefault timeMs
                            }
                  }
                    |> Player.recomputeMesh timeMs
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
        [ Html.li [] [ Html.text <| "arrow keys: " ++ Debug.toString model.arrowKeys ]
        , Html.li []
            [ Html.text <|
                "player: "
                    ++ Debug.toString
                        ( ( model.player.x
                          , model.player.y
                          , model.player.direction
                          )
                        , model.player.movement
                        )
            ]
        , Html.li [] [ Html.text <| "camera: " ++ Debug.toString model.camera ]
        ]


viewGame : Model -> Html msg
viewGame model =
    case model.tileset of
        Nothing ->
            Html.text "Loading tileset..."

        Just tileset ->
            Map.webGLEntities
                model.player.mesh
                tileset
                Camera.projection
                (Camera.translateMatrix model.camera)
                |> WebGL.toHtml
                    [ Html.Attributes.width zoomedCanvasWidthPx
                    , Html.Attributes.height zoomedCanvasHeightPx
                    ]
