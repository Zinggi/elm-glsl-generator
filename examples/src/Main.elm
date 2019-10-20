module Main exposing (main)

import Browser
import GLSL exposing (..)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Physical exposing (physical)


type alias Model =
    { withTextures : Bool }


init : Model
init =
    { withTextures = False }


type Msg
    = ToggleWithTextures


update : Msg -> Model -> Model
update msg model =
    case msg of
        ToggleWithTextures ->
            { model | withTextures = not model.withTextures }


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.div []
            [ Html.text "With textures: "
            , Html.input [ Attr.type_ "checkbox", Events.onInput (\_ -> ToggleWithTextures) ]
                []
            ]
        , Html.hr [] []
        , Html.div [ Attr.style "white-space" "pre" ]
            [ Html.text (genCode (physical model.withTextures)) ]
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }
