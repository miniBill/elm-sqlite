module SQLite.Expr exposing
    ( Expr(..), LiteralValue(..), literalValueToString, literalValueParser, parser, toRope
    , add, call, columnName, int, literalValueToRope, lt
    )

{-|

@docs Expr, LiteralValue, literalValueToString, literalValueParser, parser, toRope

-}

import Bytes exposing (Bytes)
import Hex.Convert
import List.NonEmpty exposing (NonEmpty)
import Parser.OfTokens as Parser exposing (Node(..), PStep(..), Parser)
import Parser.Token as Token exposing (Token)
import Rope exposing (Rope)
import Rope.Extra
import SQLite.Types exposing (AscDesc, ColumnName, FirstLast, SchemaName, TableName)


type Expr
    = LiteralValue LiteralValue
    | ColumnName (Maybe SchemaName) (Maybe TableName) ColumnName
    | Call FunctionName FunctionArguments (Maybe FilterClause) (Maybe OverClause)
    | Binary Expr BinaryOperator Expr
    | OTHERS Never


type alias FunctionName =
    String


type FunctionArguments
    = FunctionArgumentsList
        { distinct : Bool
        , expressions : NonEmpty Expr
        , orderBy : Maybe (NonEmpty OrderingTerm)
        }
    | FunctionArgumentsStar
    | FunctionArgumentsEmpty


type alias OrderingTerm =
    { expr : Expr
    , collate : Maybe CollationName
    , order : AscDesc
    , nulls : Maybe FirstLast
    }


type alias CollationName =
    String


type alias FilterClause =
    Never


type alias OverClause =
    Never


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


type BinaryOperator
    = Plus
    | Lt


toRope : Expr -> Rope String
toRope expr =
    case expr of
        LiteralValue literal ->
            Rope.singleton (literalValueToString literal)

        ColumnName schemaName tableName n ->
            Rope.empty
                |> Rope.Extra.appendMaybe
                    (\s -> Rope.singleton (s ++ "."))
                    schemaName
                |> Rope.Extra.appendMaybe
                    (\t -> Rope.singleton (t ++ "."))
                    tableName
                |> Rope.append n

        Call name args filter over ->
            Rope.singleton name
                |> Rope.prependTo (functionArgumentsToRope args)
                |> Rope.Extra.appendMaybe never filter
                |> Rope.Extra.appendMaybe never over

        Binary l op r ->
            Rope.singleton "("
                |> Rope.prependTo (toRope l)
                |> Rope.append (" " ++ binaryOperatorToString op ++ " ")
                |> Rope.prependTo (toRope r)
                |> Rope.append ")"

        OTHERS ever ->
            never ever


binaryOperatorToString : BinaryOperator -> String
binaryOperatorToString op =
    case op of
        Plus ->
            "+"

        Lt ->
            "<"


functionArgumentsToRope : FunctionArguments -> Rope FunctionName
functionArgumentsToRope args =
    case args of
        FunctionArgumentsStar ->
            Rope.singleton "*"

        FunctionArgumentsList _ ->
            Debug.todo "branch 'FunctionArgumentsList _' not implemented"

        FunctionArgumentsEmpty ->
            Debug.todo "branch 'FunctionArgumentsEmpty' not implemented"


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


call : FunctionName -> List Expr -> Expr
call name args =
    Call name
        (case args of
            [] ->
                FunctionArgumentsEmpty

            h :: t ->
                FunctionArgumentsList { distinct = False, expressions = ( h, t ), orderBy = Nothing }
        )
        Nothing
        Nothing


int : Int -> Expr
int i =
    LiteralValue (NumericLiteral (toFloat i))


columnName : String -> Expr
columnName v =
    ColumnName Nothing Nothing v


add : Expr -> Expr -> Expr
add l r =
    Binary l Plus r


lt : Expr -> Expr -> Expr
lt l r =
    Binary l Lt r
