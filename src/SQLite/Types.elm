module SQLite.Types exposing (AscDesc(..), ConflictClause(..), Type(..), ascDescToString, ropeToString, typeToString)

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
                    ( if String.isEmpty acc || String.endsWith " " acc || String.endsWith "\n" acc then
                        acc ++ e ++ "\n" ++ toIndentationString (indentation + 1)

                      else
                        acc ++ " " ++ e ++ "\n" ++ toIndentationString (indentation + 1)
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
                    ( if String.isEmpty acc || String.endsWith " " acc || String.endsWith "\n" acc then
                        acc ++ e

                      else
                        acc ++ " " ++ e
                    , indentation
                    )
        )
        ( "", 0 )
        input
        |> Tuple.first


type ConflictClause
    = Rollback
    | Abort
    | Fail
    | Ignore
    | Replace
