module ParserTest exposing (suite)

import Expect
import Fuzz exposing (Fuzzer)
import List.Extra
import Parser.OfTokens as Parser exposing (Node(..))
import Parser.Token as Token exposing (Token)
import Parser.Tokenizer
import SQLite.Expr
import SQLite.Statement as Statement
import SQLite.Statement.CreateTable as CreateTable
import SQLite.Types
import Test exposing (Test, fuzz)


suite : Test
suite =
    fuzz statementFuzzer "toString >> parse === Ok" <|
        \statement ->
            let
                statementString : String
                statementString =
                    statement
                        |> Statement.toString
                        |> String.split "\n"
                        |> List.Extra.removeWhen (\line -> line |> String.trim |> String.isEmpty)
                        |> String.join "\n"
            in
            case Parser.Tokenizer.tokenizer statementString of
                Ok tokenized ->
                    let
                        parsed : Result (List (Parser.DeadEnd Token)) Statement.Statement
                        parsed =
                            tokenized
                                |> Parser.run (Statement.parser |> Parser.skip Parser.end)
                    in
                    if parsed == Ok statement then
                        Expect.pass

                    else
                        [ view "Statement" identity statementString
                        , view "Tokenized" tokenizedToString tokenized
                        , view "Parsed" (parseResultToString statementString) parsed
                        ]
                            |> String.join "\n"
                            |> Expect.fail

                Err e ->
                    Expect.fail ("Failed to tokenize: " ++ e)


parseResultToString : String -> Result (List (Parser.DeadEnd Token)) Statement.Statement -> String
parseResultToString input result =
    case result of
        Err e ->
            let
                lines : List String
                lines =
                    String.split "\n" input
            in
            e
                |> List.map
                    (\{ row, column, problem } ->
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
                        in
                        pre
                            ++ current
                            ++ "\n"
                            ++ String.repeat (column - 1) " "
                            ++ "^-- "
                            ++ Debug.toString problem
                            ++ "\n"
                            ++ post
                    )
                |> String.join "\n\n--- OR ---\n\n"

        Ok s ->
            Statement.toString s


tokenizedToString : List (Node Token) -> String
tokenizedToString tokens =
    tokens
        |> List.map (\(Node _ t) -> Token.toString t)
        |> String.join " "


view : String -> (a -> String) -> a -> String
view label toString value =
    label ++ ":\n" ++ indent 4 (toString value)


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


typeFuzzer : Fuzzer SQLite.Types.Type
typeFuzzer =
    Fuzz.oneOfValues
        [ SQLite.Types.Integer
        , SQLite.Types.Numeric
        , SQLite.Types.Real
        , SQLite.Types.Text
        , SQLite.Types.Blob
        , SQLite.Types.Any
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
            CreateTable.GeneratedAs
            (Fuzz.map (\always -> { always = always }) Fuzz.bool)
            exprFuzzer
            (Fuzz.maybe generatedColumnStorageFuzzer)
        ]


ascDescFuzzer : Fuzzer SQLite.Types.AscDesc
ascDescFuzzer =
    Fuzz.oneOfValues
        [ SQLite.Types.Asc
        , SQLite.Types.Desc
        ]


conflictClauseFuzzer : Fuzzer SQLite.Types.ConflictClause
conflictClauseFuzzer =
    Fuzz.oneOfValues
        [ SQLite.Types.Rollback
        , SQLite.Types.Abort
        , SQLite.Types.Fail
        , SQLite.Types.Ignore
        , SQLite.Types.Replace
        ]


exprFuzzer : Fuzzer SQLite.Expr.Expr
exprFuzzer =
    Fuzz.oneOf
        [ Fuzz.map SQLite.Expr.LiteralValue literalValueFuzzer

        -- , Fuzz.map SQLite.Expr.OTHERS neverFuzzer
        ]


literalValueFuzzer : Fuzzer SQLite.Expr.LiteralValue
literalValueFuzzer =
    Fuzz.oneOf
        [ Fuzz.map SQLite.Expr.NumericLiteral Fuzz.float
        , Fuzz.map SQLite.Expr.StringLiteral Fuzz.string

        -- , Fuzz.map SQLite.Expr.BlobLiteral bytesFuzzer
        , Fuzz.constant SQLite.Expr.Null
        , Fuzz.constant SQLite.Expr.True_
        , Fuzz.constant SQLite.Expr.False_
        , Fuzz.constant SQLite.Expr.CurrentTime
        , Fuzz.constant SQLite.Expr.CurrentDate
        , Fuzz.constant SQLite.Expr.CurrentTimestamp
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



-- selectStatementFuzzer : Fuzzer SQLite.Statement.Select.Statement
-- selectStatementFuzzer =
--     Fuzz.oneOf
--         [Fuzz.map SQLite.Statement.Select.Statement neverFuzzer
--         ]
