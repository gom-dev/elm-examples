import Html exposing (Html, fieldset, input, label, text)
import Html.App exposing (beginnerProgram)
import Html.Attributes exposing (style, type')
import Html.Events exposing (onClick)

main =
  beginnerProgram { model = optOut, view = view, update = update}

type alias Model =
  { notifications : Bool
  , autoplay : Bool
  , location : Bool
  }

optOut : Model
optOut = 
  Model True True True

type Msg
  = ToggleNotifications
  | ToggleAutoplay
  | ToggleLocation

update : Msg -> Model -> Model
update msg model =
  case msg of
    ToggleNotifications ->
      { model | notifications = not model.notifications }
    ToggleAutoplay ->
      { model | autoplay = not model.autoplay }
    ToggleLocation ->
      { model | location = not model.location }

view : Model -> Html Msg
view model =
  fieldset []
    [ checkbox ToggleNotifications "EmailNotifications"
    , checkbox ToggleAutoplay "Video Autoplay"
    , checkbox ToggleLocation "Use Location"
    ]

checkbox : msg -> String -> Html msg
checkbox msg name =
  label
    [ style [("padding", "20px")] ]
    [ input [type' "checkbox", onClick msg] []
    , text name
    ]