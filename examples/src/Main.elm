module Main exposing (main)

import Browser
import GLSL exposing (..)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Physical exposing (physicalFrag, smoothVertex)


type alias Model =
    { withTextures : Bool
    , genElm : Bool
    }


init : Model
init =
    { withTextures = False, genElm = False }


type Msg
    = ToggleWithTextures
    | ToggleGenElm


update : Msg -> Model -> Model
update msg model =
    case msg of
        ToggleWithTextures ->
            { model | withTextures = not model.withTextures }

        ToggleGenElm ->
            { model | genElm = not model.genElm }


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.div []
            [ Html.text "With textures: "
            , Html.input [ Attr.type_ "checkbox", Events.onInput (\_ -> ToggleWithTextures) ]
                []
            ]
        , Html.div []
            [ Html.text "Generate Elm code: "
            , Html.input [ Attr.type_ "checkbox", Events.onInput (\_ -> ToggleGenElm) ]
                []
            ]
        , Html.hr [] []
        , Html.h2 [] [ Html.text "Vertex" ]
        , showCode "smoothVertex" smoothVertex model
        , Html.hr [] []
        , Html.h2 [] [ Html.text "Fragment" ]
        , showCode "physicalFragment" physicalFrag model
        ]


showCode shaderName code { genElm, withTextures } =
    Html.pre []
        [ Html.text
            (if genElm then
                generateElm shaderName (code withTextures)

             else
                generateGLSL (code withTextures)
            )
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }
