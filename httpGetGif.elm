module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as App
import Http exposing (..)
import Task
import Json.Decode as Json


type alias Model =
    { topic : String
    , gifUrl : String
    }


type Msg
    = MorePlease
    | UpdateTopic String
    | FetchSucceed String
    | FetchFail Http.Error


main : Program Never
main =
    App.program
        { init = init "cats"
        , view = view
        , update = update
        , subscriptions = \n -> Sub.none
        }


init : String -> ( Model, Cmd Msg )
init topic =
    ( Model topic "", getRandomGif topic )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MorePlease ->
            ( model, getRandomGif model.topic )

        FetchSucceed newUrl ->
            ( Model model.topic newUrl, Cmd.none )

        FetchFail _ ->
            ( model, Cmd.none )

        UpdateTopic newTopic ->
            { model | topic = newTopic } ! []


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text model.topic ]
        , input
            [ type' "text"
            , value model.topic
            , onInput UpdateTopic
            ]
            []
        , img [ src model.gifUrl ] []
        , button [ onClick MorePlease ] [ text "More Please!" ]
        ]


getRandomGif : String -> Cmd Msg
getRandomGif topic =
    let
        url =
            "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=" ++ topic
    in
        Task.perform FetchFail FetchSucceed (Http.get decodeGifUrl url)


decodeGifUrl : Json.Decoder String
decodeGifUrl =
    Json.at [ "data", "image_url" ] Json.string
