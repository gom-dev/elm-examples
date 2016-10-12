module Main exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Mouse exposing (Position)


main : Program Never
main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    Position


type Msg
    = MouseMove Position


init : ( Model, Cmd msg )
init =
    ( Position 0 0, Cmd.none )


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        MouseMove newPos ->
            newPos ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    Mouse.moves MouseMove


view : Model -> Html msg
view model =
    div
        [ style
            [ ( "position", "absolute" )
            , ( "left", toString model.x ++ "px" )
            , ( "top", toString model.y ++ "px" )
            ]
        ]
        [ text <| mousePositionText model ]


mousePositionText : Position -> String
mousePositionText pos =
    "(" ++ toString (pos.x) ++ ", " ++ toString (pos.y) ++ ")"
