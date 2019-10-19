module Main exposing (main)

import Browser
import GLSL exposing (..)
import Html exposing (Html)
import Html.Attributes as Attr
import Physical exposing (physical)


type alias Model =
    ()


init : Model
init =
    ()


type Msg
    = Msg1
    | Msg2


update : Msg -> Model -> Model
update msg model =
    model


view : Model -> Html Msg
view model =
    Html.div [ Attr.style "white-space" "pre" ]
        [ Html.text (genCode (physical True)) ]


simple =
    let
        intensity =
            uniform "float" "intensity"

        intensitySquared =
            intensity >> s " * " >> intensity

        color =
            uniform "vec3" "color"
    in
    start
        |> (s "vec3 a = vec3(gammaCorrect(1.3), " >> intensitySquared >> s ", 1.0);")
        |> (s "gl_FragColor = vec4(" >> color >> s " * a, 1.0);")


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }
