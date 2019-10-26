module GLSL exposing
    ( Generator, emptyFragmentShader, emptyVertexShader
    , GLSLType, defineAttribute, defineUniform, defineVarying, define
    , s, attribute, uniform, varying
    , generateGLSL, generateElm
    )

{-| This package helps to combine GLSL code snippets.

This package is build around functions of type `Generator -> Generator`.
They are all meant to be used in a pipeline (`|>`) or by composing them (`>>`).

@docs Generator, emptyFragmentShader, emptyVertexShader

Shaders usually start by defining variables and functions

    emptyFragmentShader
        |> defineUniform "vec3" "color"
        |> define glslCodeDefiningAFunction

@docs GLSLType, defineAttribute, defineUniform, defineVarying, define

Then you define the code for main using code snippets and possibly some more variables.
Variables used here will be automatically declared in the genrated code.

    code
        |> (s "vec4 c = vec4(color * " >> uniform "float" "colorScale" >> ", 1.0);")
        |> s "gl_FragColor = c;"

@docs s, attribute, uniform, varying

Then you take a finished `Generator` and generate code from it:

@docs generateGLSL, generateElm

-}

import Dict exposing (Dict)


{-| A type that describes a fragment shader
-}
type Generator
    = Generator
        { variables : Dict String InputType
        , code : List String
        , definitions : List String
        , isFrag : Bool
        }


type InputType
    = Uniform String
    | Varying String
    | Attribute String


{-| This starts a fragment shader pipeline
-}
emptyFragmentShader : Generator
emptyFragmentShader =
    Generator { variables = Dict.empty, code = [], definitions = [], isFrag = True }


{-| Starts a vertex shader
-}
emptyVertexShader : Generator
emptyVertexShader =
    Generator { variables = Dict.empty, code = [], definitions = [], isFrag = False }



--


{-| -}
define : String -> Generator -> Generator
define code (Generator state) =
    Generator { state | definitions = code :: state.definitions }


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


{-| -}
defineAttribute : GLSLType -> String -> Generator -> Generator
defineAttribute type_ =
    defineVariable (Attribute type_)


defineVariable : InputType -> String -> Generator -> Generator
defineVariable type_ vName (Generator state) =
    Generator { state | variables = Dict.insert vName type_ state.variables }



--


{-| Adds some lines of code to main
-}
s : String -> Generator -> Generator
s code (Generator state) =
    let
        c =
            if List.any (\x -> String.endsWith x code) [ ";", "{", "}" ] then
                code ++ "\n"

            else
                code
    in
    Generator { state | code = c :: state.code }


{-| -}
uniform : GLSLType -> String -> Generator -> Generator
uniform type_ =
    variable (Uniform type_)


{-| -}
varying : GLSLType -> String -> Generator -> Generator
varying type_ =
    variable (Varying type_)


{-| -}
attribute : GLSLType -> String -> Generator -> Generator
attribute type_ =
    variable (Attribute type_)


variable : InputType -> String -> Generator -> Generator
variable type_ vName (Generator state) =
    Generator
        { state
            | variables = Dict.insert vName type_ state.variables
            , code = vName :: state.code
        }



--


{-| Transform a generator into the finished elm code
-}
generateElm : String -> Generator -> String
generateElm name (Generator state) =
    let
        glsl =
            generateGLSL (Generator state)

        ( attributes, uniforms, varyings ) =
            Dict.foldl
                (\vName v ( aAcc, uAcc, vAcc ) ->
                    case v of
                        Attribute t ->
                            ( (vName ++ ": " ++ toElmType t) :: aAcc, uAcc, vAcc )

                        Uniform t ->
                            ( aAcc, (vName ++ ": " ++ toElmType t) :: uAcc, vAcc )

                        Varying t ->
                            ( aAcc, uAcc, (vName ++ ": " ++ toElmType t) :: vAcc )
                )
                ( [], [], [] )
                state.variables

        toAnnotation rec types =
            if List.isEmpty types then
                "{}"

            else if rec == "" then
                "{ " ++ String.join "\n, " types ++ "\n}"

            else
                "{ " ++ rec ++ "\n    | " ++ String.join "\n    , " types ++ "\n}"

        typeAnnotation =
            (if state.isFrag then
                " {}\n"

             else
                "\n" ++ indent 8 (toAnnotation "attributes" attributes) ++ "\n"
            )
                ++ indent 8 (toAnnotation "uniforms" uniforms)
                ++ "\n"
                ++ indent 8 (toAnnotation "" varyings)
    in
    (name ++ " :\n    WebGL.Shader" ++ typeAnnotation ++ "\n")
        ++ (name ++ " =\n")
        ++ "    [glsl|\n"
        ++ glsl
        ++ "|]\n"


{-| Transform a generator into the finished GLSL code
-}
generateGLSL : Generator -> String
generateGLSL (Generator state) =
    let
        variables =
            Dict.foldl
                (\vName v acc ->
                    case v of
                        Attribute t ->
                            acc ++ "attribute " ++ t ++ " " ++ vName ++ ";\n"

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
