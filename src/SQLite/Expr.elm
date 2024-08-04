module SQLite.Expr exposing
    ( Expr(..), LiteralValue(..), literalValueToString, literalValueParser, parser, toRope
    , literalValueToRope
    )

{-|

@docs Expr, LiteralValue, literalValueToString, literalValueParser, parser, toRope

-}

import Bytes exposing (Bytes)
import Hex.Convert
import Parser.OfTokens as Parser exposing (Node(..), PStep(..), Parser)
import Parser.Token as Token exposing (Token)
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
    Parser.oneOf
        [ Parser.map LiteralValue literalValueParser
        , Parser.problem "Expr.parser"
        ]


literalValueParser : Parser Token LiteralValue
literalValueParser =
    Parser.custom
        (\position stream ->
            case stream of
                (Node range (Token.Number f)) :: tail ->
                    Good True (NumericLiteral f) range.end tail

                (Node range (Token.String s)) :: tail ->
                    Good True (StringLiteral s) range.end tail

                (Node range Token.Null) :: tail ->
                    Good True Null range.end tail

                (Node range (Token.Ident i)) :: tail ->
                    case String.toUpper i of
                        "TRUE" ->
                            Good True True_ range.end tail

                        "FALSE" ->
                            Good True False_ range.end tail

                        _ ->
                            Parser.errorAt False position (Parser.Problem "Expr.literalValueParser")

                (Node range Token.Current_Date) :: tail ->
                    Good True CurrentDate range.end tail

                (Node range Token.Current_Time) :: tail ->
                    Good True CurrentTime range.end tail

                (Node range Token.Current_Timestamp) :: tail ->
                    Good True CurrentTimestamp range.end tail

                _ ->
                    Parser.errorAt False position (Parser.Problem "Expr.literalValueParser")
        )


literalValueToRope : LiteralValue -> Rope String
literalValueToRope literalValue =
    Rope.singleton (literalValueToString literalValue)
