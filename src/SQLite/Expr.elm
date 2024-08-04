module SQLite.Expr exposing (Expr(..), LiteralValue(..), literalValueToString, parser, toRope)

{-|

@docs Expr, LiteralValue, literalValueToString, parser, toRope

-}

import Bytes exposing (Bytes)
import Hex.Convert
import Parser.OfTokens as Parser exposing (Parser)
import Parser.Token exposing (Token)
import Rope exposing (Rope)


type Expr
    = LiteralValue LiteralValue
    | OTHERS Never


type LiteralValue
    = NumericLiteral Float
    | StringLiteral String
    | BlobLiteral Bytes
    | Null
    | True_
    | False_
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

        True_ ->
            "TRUE"

        False_ ->
            "FALSE"

        CurrentTime ->
            "CURRENT_TIME"

        CurrentDate ->
            "CURRENT_DATE"

        CurrentTimestamp ->
            "CURRENT_TIMESTAMP"


parser : Parser Token Expr
parser =
    Parser.problem "Expr.parser"
