module ParserTest exposing (suite)

import Ansi.Color
import Expect
import Fuzz exposing (Fuzzer)
import List.Extra
import Parser.OfTokens as Parser exposing (Node(..), Parser)
import Parser.Token as Token exposing (Token)
import Parser.Tokenizer
import Result.Extra
import Rope exposing (Rope)
import SQLite.Expr as Expr
import SQLite.Statement as Statement
import SQLite.Statement.CreateTable as CreateTable
import SQLite.Types as Types
import Test exposing (Test, describe, fuzz)


suite : Test
suite =
    describe "toString >> parse === Ok"
        [ checkRoundtrip "Statement" statementFuzzer Statement.toRope Statement.parser
        , checkRoundtrip "Column definition" columnDefinitionFuzzer CreateTable.columnDefinitionToRope CreateTable.columnDefinitionParser
        , Test.only (checkRoundtrip "Literal value" literalValueFuzzer Expr.literalValueToRope Expr.literalValueParser)
        ]


checkRoundtrip : String -> Fuzzer a -> (a -> Rope String) -> Parser Token a -> Test
checkRoundtrip label fuzzer toRope parser =
    fuzz fuzzer label <|
        \value ->
            let
                valueString : String
                valueString =
                    value
                        |> toRope
                        |> Types.ropeToString
                        |> String.split "\n"
                        |> List.Extra.removeWhen (\line -> line |> String.trim |> String.isEmpty)
                        |> String.join "\n"

                valueView : String
                valueView =
                    valueString ++ "\n" ++ Ansi.Color.fontColor Ansi.Color.brightBlack (Debug.toString value)
            in
            case Parser.Tokenizer.tokenizer valueString of
                Ok tokenized ->
                    let
                        parsed : Result (List (Parser.DeadEnd Token)) a
                        parsed =
                            tokenized
                                |> Parser.run (parser |> Parser.skip Parser.end)
                    in
                    if parsed == Ok value then
                        Expect.pass

                    else
                        [ view label valueView
                        , view "Tokenized" (tokenizedToString tokenized)
                        , view "Parsed" (parseResultToString toRope valueString parsed)
                        ]
                            |> String.join "\n"
                            |> Expect.fail

                Err ( location, e ) ->
                    let
                        lines : List String
                        lines =
                            String.split "\n" valueString
                    in
                    [ view label valueView
                    , view "Tokenized" (viewProblem lines location.row location.column [ e ])
                    ]
                        |> String.join "\n"
                        |> Expect.fail


parseResultToString : (a -> Rope String) -> String -> Result (List (Parser.DeadEnd Token)) a -> String
parseResultToString toRope input result =
    case result of
        Err errs ->
            let
                lines : List String
                lines =
                    String.split "\n" input
            in
            errs
                |> List.Extra.gatherEqualsBy (\{ row, column } -> ( row, column ))
                |> List.map
                    (\( { row, column, problem }, others ) ->
                        let
                            ( problems, expecting ) =
                                (problem :: List.map .problem others)
                                    |> List.map errorToString
                                    |> Result.Extra.partition

                            expectingMessage : Maybe String
                            expectingMessage =
                                case expecting of
                                    [] ->
                                        Nothing

                                    [ e ] ->
                                        Just ("Expecting: " ++ e)

                                    es ->
                                        Just ("Expecting one of " ++ String.join " or " es)

                            problemsMessages : List String
                            problemsMessages =
                                List.map (\p -> "Problem: " ++ p) problems

                            allMessages : List String
                            allMessages =
                                case expectingMessage of
                                    Nothing ->
                                        problemsMessages

                                    Just msg ->
                                        msg :: problemsMessages
                        in
                        viewProblem lines row column allMessages
                    )
                |> String.join "\n\n--- OR ---\n\n"

        Ok s ->
            Types.ropeToString (toRope s) ++ "\n" ++ Ansi.Color.fontColor Ansi.Color.brightBlack (Debug.toString s)


errorToString : Parser.Error Token -> Result String String
errorToString error =
    case error of
        Parser.Problem p ->
            Ok p

        Parser.ExpectingEnd ->
            Err "end"

        Parser.ExpectingToken t ->
            Err (Token.toString t)


viewProblem : List String -> Int -> Int -> List String -> String
viewProblem lines row column messages =
    let
        pre : String
        pre =
            lines
                |> List.drop (row - 3)
                |> List.take (min 2 (row - 1))
                |> List.map (\line -> line ++ "\n")
                |> String.concat

        current : String
        current =
            lines
                |> List.drop (row - 1)
                |> List.take 1
                |> String.join "\n"

        post : String
        post =
            lines
                |> List.drop row
                |> List.take 2
                |> String.join "\n"

        addMarker : Int -> String -> String
        addMarker index problemLine =
            if index == 0 then
                String.repeat (column - 1) " " ++ Ansi.Color.fontColor Ansi.Color.brightRed "^-- " ++ problemLine

            else
                String.repeat (column + 3) " " ++ problemLine
    in
    pre
        ++ current
        ++ "\n"
        ++ (messages
                |> List.indexedMap addMarker
                |> String.join "\n"
           )
        ++ "\n"
        ++ post


tokenizedToString : List (Node Token) -> String
tokenizedToString tokens =
    tokens
        |> List.map (\(Node _ t) -> Token.toString t ++ " [" ++ Debug.toString t ++ "]")
        |> String.join " "


view : String -> String -> String
view label value =
    Ansi.Color.fontColor Ansi.Color.cyan label
        ++ ":\n"
        ++ indent 4 value


indent : Int -> String -> String
indent count input =
    let
        i : String
        i =
            String.repeat count " "
    in
    input
        |> String.split "\n"
        |> List.map (\line -> i ++ line)
        |> String.join "\n"


statementFuzzer : Fuzzer Statement.Statement
statementFuzzer =
    Fuzz.map2 Statement.Statement
        (Fuzz.maybe explainFuzzer)
        innerStatementFuzzer


explainFuzzer : Fuzzer Statement.Explain
explainFuzzer =
    Fuzz.oneOfValues
        [ Statement.Explain
        , Statement.ExplainQueryPlan
        ]


innerStatementFuzzer : Fuzzer Statement.InnerStatement
innerStatementFuzzer =
    Fuzz.oneOf
        [ {- Fuzz.map Statement.AlterTable alterTableFuzzer
             , Fuzz.map Statement.Analyze analyzeFuzzer
             , Fuzz.map Statement.Attach attachFuzzer
             , Fuzz.map Statement.Begin beginFuzzer
             , Fuzz.map Statement.Commit commitFuzzer
             , Fuzz.map Statement.CreateIndex createIndexFuzzer
             ,
          -}
          Fuzz.map Statement.CreateTable createTableFuzzer

        -- , Fuzz.map Statement.CreateTrigger createTriggerFuzzer
        -- , Fuzz.map Statement.CreateView createViewFuzzer
        -- , Fuzz.map Statement.CreateVirtualTable createVirtualTableFuzzer
        -- , Fuzz.map Statement.Delete deleteFuzzer
        -- , Fuzz.map Statement.DeleteLimited deleteLimitedFuzzer
        -- , Fuzz.map Statement.Detach detachFuzzer
        -- , Fuzz.map Statement.DropIndex dropIndexFuzzer
        -- , Fuzz.map Statement.DropTable dropTableFuzzer
        -- , Fuzz.map Statement.DropTrigger dropTriggerFuzzer
        -- , Fuzz.map Statement.DropView dropViewFuzzer
        -- , Fuzz.map Statement.Insert insertFuzzer
        -- , Fuzz.map Statement.Pragma pragmaFuzzer
        -- , Fuzz.map Statement.Reindex reindexFuzzer
        -- , Fuzz.map Statement.Release releaseFuzzer
        -- , Fuzz.map Statement.Rollback rollbackFuzzer
        -- , Fuzz.map Statement.Savepoint savepointFuzzer
        -- , Fuzz.map Statement.Select selectFuzzer
        -- , Fuzz.map Statement.Update updateFuzzer
        -- , Fuzz.map Statement.UpdateLimited updateLimitedFuzzer
        -- , Fuzz.map Statement.Vacuum vacuumFuzzer
        ]


createTableFuzzer : Fuzzer CreateTable.Statement
createTableFuzzer =
    Fuzz.map5 CreateTable.Statement
        Fuzz.bool
        Fuzz.bool
        (Fuzz.maybe idFuzzer)
        idFuzzer
        createTableDefinitionFuzzer


createTableDefinitionFuzzer : Fuzzer CreateTable.TableDefinition
createTableDefinitionFuzzer =
    Fuzz.oneOf
        [ Fuzz.map
            CreateTable.TableDefinitionColumns
            (Fuzz.map3
                (\columns constraints options -> { columns = columns, constraints = constraints, options = options })
                (Fuzz.listOfLengthBetween 0 4 columnDefinitionFuzzer)
                (Fuzz.listOfLengthBetween 0 4 tableConstraintFuzzer)
                tableOptionsFuzzer
            )

        -- , Fuzz.map CreateTable.TableDefinitionSelect selectStatementFuzzer
        ]


columnDefinitionFuzzer : Fuzzer CreateTable.ColumnDefinition
columnDefinitionFuzzer =
    Fuzz.map3 CreateTable.ColumnDefinition
        idFuzzer
        (Fuzz.maybe typeFuzzer)
        (Fuzz.listOfLengthBetween 0 4 columnConstraintFuzzer)


typeFuzzer : Fuzzer Types.Type
typeFuzzer =
    Fuzz.oneOfValues
        [ Types.Integer
        , Types.Numeric
        , Types.Real
        , Types.Text
        , Types.Blob
        , Types.Any
        ]


columnConstraintFuzzer : Fuzzer CreateTable.ColumnConstraint
columnConstraintFuzzer =
    Fuzz.map2 CreateTable.ColumnConstraint
        (Fuzz.maybe idFuzzer)
        innerColumnConstraintFuzzer


innerColumnConstraintFuzzer : Fuzzer CreateTable.InnerColumnConstraint
innerColumnConstraintFuzzer =
    Fuzz.oneOf
        [ Fuzz.map3
            CreateTable.ColumnPrimaryKey
            (Fuzz.maybe ascDescFuzzer)
            (Fuzz.maybe conflictClauseFuzzer)
            (Fuzz.map (\autoIncrement -> { autoIncrement = autoIncrement }) Fuzz.bool)
        , Fuzz.map CreateTable.ColumnNotNull (Fuzz.maybe conflictClauseFuzzer)
        , Fuzz.map CreateTable.ColumnUnique (Fuzz.maybe conflictClauseFuzzer)
        , Fuzz.map CreateTable.ColumnCheck exprFuzzer
        , Fuzz.map CreateTable.ColumnDefault exprFuzzer
        , Fuzz.map CreateTable.ColumnCollate idFuzzer
        , Fuzz.map CreateTable.ColumnForeignKey foreignKeyClauseFuzzer
        , Fuzz.map3
            CreateTable.ColumnGeneratedAs
            (Fuzz.map (\always -> { always = always }) Fuzz.bool)
            exprFuzzer
            (Fuzz.maybe generatedColumnStorageFuzzer)
        ]


ascDescFuzzer : Fuzzer Types.AscDesc
ascDescFuzzer =
    Fuzz.oneOfValues
        [ Types.Asc
        , Types.Desc
        ]


conflictClauseFuzzer : Fuzzer Types.ConflictClause
conflictClauseFuzzer =
    Fuzz.oneOfValues
        [ Types.Rollback
        , Types.Abort
        , Types.Fail
        , Types.Ignore
        , Types.Replace
        ]


exprFuzzer : Fuzzer Expr.Expr
exprFuzzer =
    Fuzz.oneOf
        [ Fuzz.map Expr.LiteralValue literalValueFuzzer

        -- , Fuzz.map Expr.OTHERS neverFuzzer
        ]


literalValueFuzzer : Fuzzer Expr.LiteralValue
literalValueFuzzer =
    Fuzz.oneOf
        [ Fuzz.map Expr.NumericLiteral (Fuzz.floatRange 0 (2 ^ 53))
        , Fuzz.map Expr.StringLiteral Fuzz.string

        -- , Fuzz.map Expr.BlobLiteral bytesFuzzer
        , Fuzz.constant Expr.Null
        , Fuzz.constant Expr.True_
        , Fuzz.constant Expr.False_
        , Fuzz.constant Expr.CurrentTime
        , Fuzz.constant Expr.CurrentDate
        , Fuzz.constant Expr.CurrentTimestamp
        ]


foreignKeyClauseFuzzer : Fuzzer CreateTable.ForeignKeyClause
foreignKeyClauseFuzzer =
    Fuzz.map6
        CreateTable.ForeignKeyClause
        idFuzzer
        (Fuzz.listOfLengthBetween 0 4 idFuzzer)
        (Fuzz.maybe onDeleteUpdateFuzzer)
        (Fuzz.maybe onDeleteUpdateFuzzer)
        (Fuzz.maybe idFuzzer)
        (Fuzz.oneOf
            [ Fuzz.constant Nothing

            -- , Fuzz.map Just neverFuzzer
            ]
        )


onDeleteUpdateFuzzer : Fuzzer CreateTable.OnDeleteUpdate
onDeleteUpdateFuzzer =
    Fuzz.oneOfValues
        [ CreateTable.SetNull
        , CreateTable.SetDefault
        , CreateTable.Cascade
        , CreateTable.Restrict
        , CreateTable.NoAction
        ]


generatedColumnStorageFuzzer : Fuzzer CreateTable.GeneratedColumnStorage
generatedColumnStorageFuzzer =
    Fuzz.oneOfValues
        [ CreateTable.Stored
        , CreateTable.Virtual
        ]


tableConstraintFuzzer : Fuzzer CreateTable.TableConstraint
tableConstraintFuzzer =
    Fuzz.map2 CreateTable.TableConstraint
        (Fuzz.maybe idFuzzer)
        innerTableConstraintFuzzer


innerTableConstraintFuzzer : Fuzzer CreateTable.InnerTableConstraint
innerTableConstraintFuzzer =
    Fuzz.oneOf
        [ Fuzz.map2 CreateTable.TablePrimaryKey
            (Fuzz.listOfLengthBetween 0 4 indexedColumnFuzzer)
            (Fuzz.maybe conflictClauseFuzzer)
        , Fuzz.map2 CreateTable.TableUnique
            (Fuzz.listOfLengthBetween 0 4 indexedColumnFuzzer)
            (Fuzz.maybe conflictClauseFuzzer)
        , Fuzz.map CreateTable.TableCheck exprFuzzer
        , Fuzz.map2 CreateTable.TableForeignKey
            (Fuzz.listOfLengthBetween 0 4 idFuzzer)
            foreignKeyClauseFuzzer
        ]


indexedColumnFuzzer : Fuzzer CreateTable.IndexedColumn
indexedColumnFuzzer =
    Fuzz.map3 CreateTable.IndexedColumn
        nameOrExprFuzzer
        (Fuzz.maybe idFuzzer)
        (Fuzz.maybe ascDescFuzzer)


nameOrExprFuzzer : Fuzzer CreateTable.NameOrExpr
nameOrExprFuzzer =
    Fuzz.oneOf
        [ Fuzz.map CreateTable.IsName idFuzzer
        , Fuzz.map CreateTable.IsExpr exprFuzzer
        ]


tableOptionsFuzzer : Fuzzer CreateTable.TableOptions
tableOptionsFuzzer =
    Fuzz.map2 CreateTable.TableOptions
        Fuzz.bool
        Fuzz.bool


idFuzzer : Fuzzer String
idFuzzer =
    List.range (Char.toCode 'a') (Char.toCode 'z')
        |> List.map Char.fromCode
        |> Fuzz.oneOfValues
        |> Fuzz.listOfLengthBetween 1 10
        |> Fuzz.map String.fromList
        |> Fuzz.map
            (\s ->
                case Token.fromString (String.toUpper s) of
                    Nothing ->
                        s

                    Just _ ->
                        s ++ "_"
            )



-- selectStatementFuzzer : Fuzzer SQLite.Statement.Select.Statement
-- selectStatementFuzzer =
--     Fuzz.oneOf
--         [Fuzz.map SQLite.Statement.Select.Statement neverFuzzer
--         ]
