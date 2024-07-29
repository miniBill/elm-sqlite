module SQLite.Expr exposing (Expr(..), LiteralValue(..), literalValueToString, toRope)

import Bytes exposing (Bytes)
import Hex.Convert
import Rope exposing (Rope)


type Expr
    = LiteralValue LiteralValue
    | OTHERS Never


type LiteralValue
    = NumericLiteral Float
    | StringLiteral String
    | BlobLiteral Bytes
    | Null
    | True
    | False
    | CurrentTime
    | CurrentDate
    | CurrentTimestamp


toRope : Expr -> Rope String
toRope expr =
    case expr of
        LiteralValue literal ->
            Rope.singleton (literalValueToString literal)

        OTHERS ever ->
            never ever


literalValueToString : LiteralValue -> String
literalValueToString literal =
    case literal of
        NumericLiteral numeric ->
            String.fromFloat numeric

        StringLiteral s ->
            "'" ++ String.replace "'" "''" s ++ "'"

        BlobLiteral bytes ->
            "X'" ++ Hex.Convert.toString bytes ++ "'"

        Null ->
            "NULL"

        True ->
            "TRUE"

        False ->
            "FALSE"

        CurrentTime ->
            "CURRENT_TIME"

        CurrentDate ->
            "CURRENT_DATE"

        CurrentTimestamp ->
            "CURRENT_TIMESTAMP"
