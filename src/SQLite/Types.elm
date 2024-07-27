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
    Rope.foldl (\e acc -> acc ++ " " ++ e) "" input
