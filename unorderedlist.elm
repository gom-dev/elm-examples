module Main exposing (..)

import Html exposing (li, text, ul)
import Html.Attributes exposing (class)


main =
    ul [ class "glocery-list" ]
        [ li [] [ text "apple" ]
        , li [] [ text "orange" ]
        , li [] [ text "eggs" ]
        , li [] [ text "banana" ]
        ]
