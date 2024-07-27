module SQLite.Types exposing (AscDesc(..), Type(..), ascDescToString, ropeToString, typeToString)

import Rope exposing (Rope)


type Type
    = Integer
    | Numeric
    | Real
    | Text
    | Blob
    | Any


type AscDesc
    = Asc
    | Desc


typeToString : Type -> String
typeToString tipe =
    case tipe of
        Integer ->
            "INTEGER"

        Numeric ->
            "NUMERIC"

        Real ->
            "REAL"

        Text ->
            "TEXT"

        Blob ->
            "BLOB"

        Any ->
            "ANY"


ascDescToString : AscDesc -> String
ascDescToString ascDesc =
    case ascDesc of
        Asc ->
            "ASC"

        Desc ->
            "DESC"


ropeToString : Rope String -> String
ropeToString input =
    let
        toIndentationString : Int -> String
        toIndentationString indentation =
            String.repeat indentation "  "
    in
    Rope.foldl
        (\e ( acc, indentation ) ->
            case e of
                "(" ->
                    ( acc ++ " " ++ e ++ "\n" ++ toIndentationString (indentation + 1)
                    , indentation + 1
                    )

                "," ->
                    ( acc ++ e ++ "\n" ++ toIndentationString indentation
                    , indentation
                    )

                ")" ->
                    ( acc ++ "\n" ++ toIndentationString (indentation - 1) ++ e
                    , indentation - 1
                    )

                _ ->
                    ( acc ++ " " ++ e
                    , indentation
                    )
        )
        ( "", 0 )
        input
        |> Tuple.first
