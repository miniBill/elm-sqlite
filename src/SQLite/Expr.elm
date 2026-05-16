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
import Parser.Extra
import Parser.OfTokens as Parser exposing (Node(..), PStep(..), Parser, token_)
import Parser.Token as Token exposing (Token)
import Rope exposing (Rope)
import Rope.Extra
import SQLite.Types exposing (AscDesc(..), ColumnName, FirstLast(..), SchemaName, TableName)


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
    , order : Maybe AscDesc
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
    = And
    | Plus
    | Eq
    | Geq
    | Gt
    | Leq
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

        Leq ->
            "<="

        Gt ->
            ">"

        Geq ->
            ">="

        Eq ->
            "="

        And ->
            "AND"


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
    Parser.succeed (|>)
        |> Parser.keep relationParser
        |> Parser.oneOf_
            [ Parser.succeed (\r l -> and l r)
                |> Parser.token_ Token.And
                |> Parser.keep relationParser
            , Parser.succeed identity
            ]


relationParser : Parser Token Expr
relationParser =
    Parser.succeed (|>)
        |> Parser.keep addSubParser
        |> Parser.oneOf_
            [ Parser.succeed (\r l -> lt l r)
                |> Parser.token_ Token.LessThan
                |> Parser.keep addSubParser
            , Parser.succeed (\r l -> leq l r)
                |> Parser.token_ Token.LessThanOrEquals
                |> Parser.keep addSubParser
            , Parser.succeed (\r l -> gt l r)
                |> Parser.token_ Token.GreaterThan
                |> Parser.keep addSubParser
            , Parser.succeed (\r l -> geq l r)
                |> Parser.token_ Token.GreaterThanOrEquals
                |> Parser.keep addSubParser
            , Parser.succeed identity
            ]


addSubParser : Parser Token Expr
addSubParser =
    Parser.succeed (|>)
        |> Parser.keep leafParser
        |> Parser.oneOf_
            [ Parser.succeed (\r l -> add l r)
                |> Parser.token_ Token.Plus
                |> Parser.keep leafParser
            , Parser.succeed identity
            ]


leafParser : Parser Token Expr
leafParser =
    Parser.oneOf
        [ Parser.map LiteralValue literalValueParser
        , Parser.problem "Bind parameter"
        , functionCallParser
        , Parser.custom
            (\position stream ->
                case stream of
                    (Node _ (Token.Ident schema)) :: (Node _ Token.Dot) :: (Node _ (Token.Ident table)) :: (Node _ Token.Dot) :: (Node columnRange (Token.Ident column)) :: tail ->
                        Parser.Good True (ColumnName (Just schema) (Just table) column) columnRange.end tail

                    (Node _ (Token.Ident table)) :: (Node _ Token.Dot) :: (Node columnRange (Token.Ident column)) :: tail ->
                        Parser.Good True (ColumnName Nothing (Just table) column) columnRange.end tail

                    (Node columnRange (Token.Ident column)) :: tail ->
                        Parser.Good True (ColumnName Nothing Nothing column) columnRange.end tail

                    _ ->
                        Parser.errorAt False position (Parser.Problem "Expecting (optionally qualified) column name")
            )
        , Parser.problem "Expr.leafParser"
        ]


functionCallParser : Parser Token Expr
functionCallParser =
    Parser.succeed Call
        |> Parser.backtrackable_ Parser.Extra.ident
        |> Parser.token_ Token.ParensOpen
        |> Parser.keep functionArgumentsParser
        |> Parser.token_ Token.ParensClose
        |> Parser.maybe_ filterClauseParser
        |> Parser.maybe_ overClauseParser


functionArgumentsParser : Parser Token FunctionArguments
functionArgumentsParser =
    Parser.oneOf
        [ Parser.succeed FunctionArgumentsStar |> Parser.token_ Token.Star
        , Parser.succeed
            (\d e o ->
                FunctionArgumentsList
                    { distinct = d
                    , expressions = e
                    , orderBy = o
                    }
            )
            |> Parser.oneOf_
                [ Parser.succeed True |> Parser.token_ Token.Distinct
                , Parser.succeed False
                ]
            |> Parser.manyWithSeparator_ Token.Comma (Parser.lazy (\() -> parser))
            |> Parser.maybe_
                (Parser.succeed identity
                    |> Parser.token_ Token.Order
                    |> Parser.token_ Token.By
                    |> Parser.manyWithSeparator_ Token.Comma orderingTermParser
                )
        , Parser.succeed FunctionArgumentsEmpty
        ]


orderingTermParser : Parser Token OrderingTerm
orderingTermParser =
    Parser.succeed OrderingTerm
        |> Parser.keep (Parser.lazy (\() -> parser))
        |> Parser.maybe_
            (Parser.succeed identity
                |> Parser.token_ Token.Collate
                |> Parser.Extra.ident_
            )
        |> Parser.oneOf_
            [ Parser.succeed (Just Asc) |> token_ Token.Asc
            , Parser.succeed (Just Desc) |> token_ Token.Desc
            , Parser.succeed Nothing
            ]
        |> Parser.oneOf_
            [ Parser.succeed Just
                |> token_ Token.Nulls
                |> Parser.oneOf_
                    [ Parser.succeed First |> token_ Token.First
                    , Parser.succeed Last |> token_ Token.Last
                    ]
            , Parser.succeed Nothing
            ]


filterClauseParser : Parser Token FilterClause
filterClauseParser =
    Parser.problem "Expr.filterClauseParser"


overClauseParser : Parser Token OverClause
overClauseParser =
    Parser.problem "Expr.overClauseParser"


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


leq : Expr -> Expr -> Expr
leq l r =
    Binary l Leq r


gt : Expr -> Expr -> Expr
gt l r =
    Binary l Gt r


geq : Expr -> Expr -> Expr
geq l r =
    Binary l Geq r


and : Expr -> Expr -> Expr
and l r =
    Binary l And r
