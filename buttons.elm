import Html exposing (div, button, text)
import Html.App exposing (beginnerProgram)
import Html.Events exposing (onClick)

main : Program Never
main =
    beginnerProgram {model = 0, view = view, update = update}

view : number -> Html.Html Msg
view model =
    div []
        [ button [onClick Decrement] [text "-"]
        , div [] [text (toString model)]
        , button [onClick Increment] [text "+"]
        , div [] [text ""]
        , button [onClick Reset] [text "reset"]
        ]

type Msg = Increment | Decrement | Reset

update : Msg -> number -> number
update msg model =
    case msg of
        Increment ->
            model + 1
        Decrement ->
            model - 1
        Reset ->
            0