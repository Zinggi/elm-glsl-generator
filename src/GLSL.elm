module GLSL exposing
    ( Generator, emptyFragmentShader
    , GLSLType, defineUniform, defineVarying, define
    , s, uniform, varying
    , generateGLSL, generateElm
    )

{-| This package helps to combine GLSL code snippets.

This package is build around functions of type `Generator -> Generator`.
They are all meant to be used in a pipeline (`|>`) or by composing them (`>>`).

@docs Generator, emptyFragmentShader

Shaders usually start by defining variables and functions

    emptyFragmentShader
        |> defineUniform "vec3" "color"
        |> define glslCodeDefiningAFunction

@docs GLSLType, defineUniform, defineVarying, define

Then you define the code for main using code snippets and possibly some more variables.
Variables used here (with `uniform` and `varying`) will be automatically declared in the genrated code.

    code
        |> (s "vec4 c = vec4(color * " >> uniform "float" "colorScale" >> ", 1.0);")
        |> s "gl_FragColor = c;"

@docs s, uniform, varying

Then you take a finished `Generator` and generate code from it:

@docs generateGLSL, generateElm

-}

import Dict exposing (Dict)


{-| A type that describes a fragment shader
-}
type Generator
    = Fragment
        { variables : Dict String InputType
        , code : List String
        , definitions : List String
        }


{-| This starts a fragment shader pipeline
-}
emptyFragmentShader : Generator
emptyFragmentShader =
    Fragment { variables = Dict.empty, code = [], definitions = [] }



--


{-| -}
define : String -> Generator -> Generator
define code (Fragment state) =
    Fragment { state | definitions = code :: state.definitions }


{-| -}
type alias GLSLType =
    String


{-| -}
defineUniform : GLSLType -> String -> Generator -> Generator
defineUniform type_ =
    defineVariable (Uniform type_)


{-| -}
defineVarying : GLSLType -> String -> Generator -> Generator
defineVarying type_ =
    defineVariable (Varying type_)



--


{-| Adds some lines of code to main
-}
s : String -> Generator -> Generator
s code (Fragment state) =
    let
        c =
            if List.any (\x -> String.endsWith x code) [ ";", "{", "}" ] then
                code ++ "\n"

            else
                code
    in
    Fragment { state | code = c :: state.code }


{-| -}
uniform : GLSLType -> String -> Generator -> Generator
uniform type_ =
    variable (Uniform type_)


{-| -}
varying : GLSLType -> String -> Generator -> Generator
varying type_ =
    variable (Varying type_)



--


{-| Transform a generator into the finished elm code
-}
generateElm : String -> Generator -> String
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


{-| Transform a generator into the finished GLSL code
-}
generateGLSL : Generator -> String
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
                    frag ++ acc
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



-- Internal


type InputType
    = Uniform String
    | Varying String


variable : InputType -> String -> Generator -> Generator
variable type_ vName (Fragment state) =
    Fragment
        { state
            | variables = Dict.insert vName type_ state.variables
            , code = vName :: state.code
        }


defineVariable : InputType -> String -> Generator -> Generator
defineVariable type_ vName (Fragment state) =
    Fragment { state | variables = Dict.insert vName type_ state.variables }


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


indent : Int -> String -> String
indent n str =
    String.lines str
        |> List.map ((++) (String.repeat n " "))
        |> String.join "\n"
