module Main exposing (..)

import Html exposing (Html, Attribute, text, div, input)
import Html.App exposing (beginnerProgram)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import String


main =
    beginnerProgram { model = "", view = view, update = update }


type Msg
    = NewContent String


update (NewContent content) oldContent =
    content


view content =
    div []
        [ input [ placeholder "Text to reverse", onInput NewContent, myStyle ] []
        , div [ myStyle ] [ text (String.reverse content) ]
        ]


myStyle =
    style
        [ ( "width", "100%" )
        , ( "height", "40px" )
        , ( "padding", "10px 0" )
        , ( "font-size", "2em" )
        , ( "text-align", "center" )
        ]
