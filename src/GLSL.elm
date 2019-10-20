module GLSL exposing
    ( Fragment
    , define
    , defineUniform
    , defineVarying
    , generateElm
    , generateGLSL
    , s
    , start
    , uniform
    , varying
    )

import Dict exposing (Dict)


type Fragment
    = Fragment
        { variables : Dict String InputType
        , code : List Frag
        , definitions : List String
        }



-- Internal


type Frag
    = Code String


type InputType
    = Uniform String
    | Varying String


uniform : String -> String -> Fragment -> Fragment
uniform type_ =
    variable (Uniform type_)


varying : String -> String -> Fragment -> Fragment
varying type_ =
    variable (Varying type_)


variable : InputType -> String -> Fragment -> Fragment
variable type_ vName (Fragment state) =
    Fragment
        { state
            | variables = Dict.insert vName type_ state.variables
            , code = Code vName :: state.code
        }


defineUniform : String -> String -> Fragment -> Fragment
defineUniform type_ =
    defineVariable (Uniform type_)


defineVarying : String -> String -> Fragment -> Fragment
defineVarying type_ =
    defineVariable (Varying type_)


defineVariable : InputType -> String -> Fragment -> Fragment
defineVariable type_ vName (Fragment state) =
    Fragment { state | variables = Dict.insert vName type_ state.variables }


define : String -> Fragment -> Fragment
define code (Fragment state) =
    Fragment { state | definitions = code :: state.definitions }


generateElm : String -> Fragment -> String
generateElm name (Fragment state) =
    let
        glsl =
            generateGLSL (Fragment state)

        ( uniforms, varyings ) =
            Dict.foldl
                (\vName v ( uAcc, vAcc ) ->
                    case v of
                        Uniform t ->
                            ( (vName ++ ": " ++ toElmType t) :: uAcc, vAcc )

                        Varying t ->
                            ( uAcc, (vName ++ ": " ++ toElmType t) :: vAcc )
                )
                ( [], [] )
                state.variables

        toAnnotation rec types =
            if List.isEmpty types then
                "{}"

            else if rec == "" then
                "{ " ++ String.join "\n, " types ++ "\n}"

            else
                "{ " ++ rec ++ "\n    | " ++ String.join "\n    , " types ++ "\n}"

        typeAnnotation =
            indent 8 (toAnnotation "uniforms" uniforms)
                ++ "\n"
                ++ indent 8 (toAnnotation "" varyings)
    in
    (name ++ " :\n    WebGL.Shader {}\n" ++ typeAnnotation ++ "\n")
        ++ (name ++ " =\n")
        ++ "    [glsl|\n"
        ++ glsl
        ++ "|]\n"


generateGLSL : Fragment -> String
generateGLSL (Fragment state) =
    let
        variables =
            Dict.foldl
                (\vName v acc ->
                    case v of
                        Uniform t ->
                            acc ++ "uniform " ++ t ++ " " ++ vName ++ ";\n"

                        Varying t ->
                            acc ++ "varying " ++ t ++ " " ++ vName ++ ";\n"
                )
                ""
                state.variables

        definitions =
            String.join "\n" (List.reverse state.definitions)

        main =
            List.foldl
                (\frag acc ->
                    case frag of
                        Code t ->
                            t ++ acc
                )
                ""
                state.code
                |> String.dropRight 1
    in
    "precision mediump float;\n\n"
        ++ variables
        ++ definitions
        ++ "\nvoid main () {\n"
        ++ indent 4 main
        ++ "\n}\n"


s : String -> Fragment -> Fragment
s code (Fragment state) =
    let
        c =
            if List.any (\x -> String.endsWith x code) [ ";", "{", "}" ] then
                code ++ "\n"

            else
                code
    in
    Fragment { state | code = Code c :: state.code }


start : Fragment
start =
    Fragment { variables = Dict.empty, code = [], definitions = [] }



--- Internal


toElmType : String -> String
toElmType glslType =
    case glslType of
        "int" ->
            "Int"

        "float" ->
            "Float"

        "vec2" ->
            "Vec2"

        "vec3" ->
            "Vec3"

        "vec4" ->
            "Vec4"

        "mat2" ->
            "Mat2"

        "mat3" ->
            "Mat3"

        "mat4" ->
            "Mat4"

        "sampler2D" ->
            "Texture"

        other ->
            other


indent n str =
    String.lines str
        |> List.map ((++) (String.repeat n " "))
        |> String.join "\n"
