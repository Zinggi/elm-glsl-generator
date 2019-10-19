module GLSL exposing
    ( Fragment
    , define
    , genCode
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


define : String -> Fragment -> Fragment
define code (Fragment state) =
    Fragment { state | definitions = code :: state.definitions }


genCode : Fragment -> String
genCode (Fragment state) =
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
            if String.endsWith ";" code || String.endsWith "{" code then
                code ++ "\n"

            else
                code
    in
    Fragment { state | code = Code c :: state.code }


start : Fragment
start =
    Fragment { variables = Dict.empty, code = [], definitions = [] }



--- Internal


indent n str =
    String.lines str
        |> List.map ((++) (String.repeat n " "))
        |> String.join "\n"
