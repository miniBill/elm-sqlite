module SQLite.Types exposing (AscDesc(..), ConflictClause(..), Type(..), ascDescToString, ropeToString, typeParser, typeToString)

import Parser.OfTokens as Parser exposing (Node(..), Parser)
import Parser.Token as Token exposing (Token)
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


typeFromString : String -> Maybe Type
typeFromString input =
    case input of
        "INTEGER" ->
            Just Integer

        "NUMERIC" ->
            Just Numeric

        "REAL" ->
            Just Real

        "TEXT" ->
            Just Text

        "BLOB" ->
            Just Blob

        "ANY" ->
            Just Any

        _ ->
            Nothing


typeParser : Parser Token Type
typeParser =
    Parser.custom
        (\location stream ->
            case stream of
                (Node range Token.Integer) :: tail ->
                    Parser.Good True Integer range.end tail

                (Node range (Token.Ident i)) :: tail ->
                    case typeFromString (String.toUpper i) of
                        Just t ->
                            Parser.Good True t range.end tail

                        _ ->
                            Parser.errorAt False location (Parser.Problem (i ++ " is not a valid type"))

                _ ->
                    Parser.errorAt False location (Parser.Problem "Expecting type")
        )


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
