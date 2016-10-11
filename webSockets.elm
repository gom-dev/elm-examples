import Html exposing (..)
import Html.App as App
import Html.Events exposing (..)
import WebSocket

main : Program Never
main =
  App.program 
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type alias Model =
  { input : String
  , messages : List String
  }

type Msg
  = Input String
  | Send
  | NewMessage String

echoServer : String
echoServer =
  "wss://echo.websocket.org"

init : (Model, Cmd Msg)
init =
  (Model "" [], Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg {input, messages} =
  case msg of
    Input newInput ->
      (Model newInput messages, Cmd.none)
    Send ->
      (Model "" messages, WebSocket.send echoServer input)
    NewMessage str ->
      (Model input (str :: messages), Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  WebSocket.listen echoServer NewMessage

view : Model -> Html Msg
view model =
  div []
    [ div [] (List.map viewMessage model.messages)
    , input [onInput Input] []
    , button [onClick Send] [text "Send"]
    ]

viewMessage : String -> Html Msg
viewMessage msg =
  div [] [text msg]