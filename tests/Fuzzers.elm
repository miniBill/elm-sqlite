module Fuzzers exposing (columnDefinition, literalValue, statement)

import Fuzz exposing (Fuzzer)
import Parser.Token as Token
import SQLite.Expr as Expr
import SQLite.Statement as Statement
import SQLite.Statement.CreateTable as CreateTable
import SQLite.Types as Types


statement : Fuzzer Statement.Statement
statement =
    Fuzz.map2 Statement.Statement
        (Fuzz.maybe explain)
        innerStatement


explain : Fuzzer Statement.Explain
explain =
    Fuzz.oneOfValues
        [ Statement.Explain
        , Statement.ExplainQueryPlan
        ]


innerStatement : Fuzzer Statement.InnerStatement
innerStatement =
    Fuzz.oneOf
        [ {- Fuzz.map Statement.AlterTable alterTable
             , Fuzz.map Statement.Analyze analyze
             , Fuzz.map Statement.Attach attach
             , Fuzz.map Statement.Begin begin
             , Fuzz.map Statement.Commit commit
             , Fuzz.map Statement.CreateIndex createIndex
             ,
          -}
          Fuzz.map Statement.CreateTable createTable

        -- , Fuzz.map Statement.CreateTrigger createTrigger
        -- , Fuzz.map Statement.CreateView createView
        -- , Fuzz.map Statement.CreateVirtualTable createVirtualTable
        -- , Fuzz.map Statement.Delete delete
        -- , Fuzz.map Statement.DeleteLimited deleteLimited
        -- , Fuzz.map Statement.Detach detach
        -- , Fuzz.map Statement.DropIndex dropIndex
        -- , Fuzz.map Statement.DropTable dropTable
        -- , Fuzz.map Statement.DropTrigger dropTrigger
        -- , Fuzz.map Statement.DropView dropView
        -- , Fuzz.map Statement.Insert insert
        -- , Fuzz.map Statement.Pragma pragma
        -- , Fuzz.map Statement.Reindex reindex
        -- , Fuzz.map Statement.Release release
        -- , Fuzz.map Statement.Rollback rollback
        -- , Fuzz.map Statement.Savepoint savepoint
        -- , Fuzz.map Statement.Select select
        -- , Fuzz.map Statement.Update update
        -- , Fuzz.map Statement.UpdateLimited updateLimited
        -- , Fuzz.map Statement.Vacuum vacuum
        ]


createTable : Fuzzer CreateTable.Statement
createTable =
    Fuzz.map5 CreateTable.Statement
        Fuzz.bool
        Fuzz.bool
        (Fuzz.maybe id)
        id
        createTableDefinition


createTableDefinition : Fuzzer CreateTable.TableDefinition
createTableDefinition =
    Fuzz.oneOf
        [ Fuzz.map
            CreateTable.TableDefinitionColumns
            (Fuzz.map3
                (\columns constraints options -> { columns = columns, constraints = constraints, options = options })
                (Fuzz.listOfLengthBetween 0 4 columnDefinition)
                (Fuzz.listOfLengthBetween 0 4 tableConstraint)
                tableOptions
            )

        -- , Fuzz.map CreateTable.TableDefinitionSelect selectStatementFuzzer
        ]


columnDefinition : Fuzzer CreateTable.ColumnDefinition
columnDefinition =
    Fuzz.map3 CreateTable.ColumnDefinition
        id
        (Fuzz.maybe type_)
        (Fuzz.listOfLengthBetween 0 4 columnConstraint)


type_ : Fuzzer Types.Type
type_ =
    Fuzz.oneOfValues
        [ Types.Integer
        , Types.Numeric
        , Types.Real
        , Types.Text
        , Types.Blob
        , Types.Any
        ]


columnConstraint : Fuzzer CreateTable.ColumnConstraint
columnConstraint =
    Fuzz.map2 CreateTable.ColumnConstraint
        (Fuzz.maybe id)
        innerColumnConstraint


innerColumnConstraint : Fuzzer CreateTable.InnerColumnConstraint
innerColumnConstraint =
    Fuzz.oneOf
        [ Fuzz.map3
            CreateTable.ColumnPrimaryKey
            (Fuzz.maybe ascDesc)
            (Fuzz.maybe conflictClause)
            (Fuzz.map (\autoIncrement -> { autoIncrement = autoIncrement }) Fuzz.bool)
        , Fuzz.map CreateTable.ColumnNotNull (Fuzz.maybe conflictClause)
        , Fuzz.map CreateTable.ColumnUnique (Fuzz.maybe conflictClause)
        , Fuzz.map CreateTable.ColumnCheck expr
        , Fuzz.map CreateTable.ColumnDefault expr
        , Fuzz.map CreateTable.ColumnCollate id
        , Fuzz.map CreateTable.ColumnForeignKey foreignKeyClause
        , Fuzz.map3
            CreateTable.ColumnGeneratedAs
            (Fuzz.map (\always -> { always = always }) Fuzz.bool)
            expr
            (Fuzz.maybe generatedColumnStorage)
        ]


ascDesc : Fuzzer Types.AscDesc
ascDesc =
    Fuzz.oneOfValues
        [ Types.Asc
        , Types.Desc
        ]


conflictClause : Fuzzer Types.ConflictClause
conflictClause =
    Fuzz.oneOfValues
        [ Types.Rollback
        , Types.Abort
        , Types.Fail
        , Types.Ignore
        , Types.Replace
        ]


expr : Fuzzer Expr.Expr
expr =
    Fuzz.oneOf
        [ Fuzz.map Expr.LiteralValue literalValue

        -- , Fuzz.map Expr.OTHERS neverFuzzer
        ]


literalValue : Fuzzer Expr.LiteralValue
literalValue =
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


foreignKeyClause : Fuzzer CreateTable.ForeignKeyClause
foreignKeyClause =
    Fuzz.map6
        CreateTable.ForeignKeyClause
        id
        (Fuzz.listOfLengthBetween 0 4 id)
        (Fuzz.maybe onDeleteUpdate)
        (Fuzz.maybe onDeleteUpdate)
        (Fuzz.maybe id)
        (Fuzz.oneOf
            [ Fuzz.constant Nothing

            -- , Fuzz.map Just neverFuzzer
            ]
        )


onDeleteUpdate : Fuzzer CreateTable.OnDeleteUpdate
onDeleteUpdate =
    Fuzz.oneOfValues
        [ CreateTable.SetNull
        , CreateTable.SetDefault
        , CreateTable.Cascade
        , CreateTable.Restrict
        , CreateTable.NoAction
        ]


generatedColumnStorage : Fuzzer CreateTable.GeneratedColumnStorage
generatedColumnStorage =
    Fuzz.oneOfValues
        [ CreateTable.Stored
        , CreateTable.Virtual
        ]


tableConstraint : Fuzzer CreateTable.TableConstraint
tableConstraint =
    Fuzz.map2 CreateTable.TableConstraint
        (Fuzz.maybe id)
        innerTableConstraint


innerTableConstraint : Fuzzer CreateTable.InnerTableConstraint
innerTableConstraint =
    Fuzz.oneOf
        [ Fuzz.map2 CreateTable.TablePrimaryKey
            (Fuzz.listOfLengthBetween 0 4 indexedColumn)
            (Fuzz.maybe conflictClause)
        , Fuzz.map2 CreateTable.TableUnique
            (Fuzz.listOfLengthBetween 0 4 indexedColumn)
            (Fuzz.maybe conflictClause)
        , Fuzz.map CreateTable.TableCheck expr
        , Fuzz.map2 CreateTable.TableForeignKey
            (Fuzz.listOfLengthBetween 0 4 id)
            foreignKeyClause
        ]


indexedColumn : Fuzzer CreateTable.IndexedColumn
indexedColumn =
    Fuzz.map3 CreateTable.IndexedColumn
        nameOrExpr
        (Fuzz.maybe id)
        (Fuzz.maybe ascDesc)


nameOrExpr : Fuzzer CreateTable.NameOrExpr
nameOrExpr =
    Fuzz.oneOf
        [ Fuzz.map CreateTable.IsName id
        , Fuzz.map CreateTable.IsExpr expr
        ]


tableOptions : Fuzzer CreateTable.TableOptions
tableOptions =
    Fuzz.map2 CreateTable.TableOptions
        Fuzz.bool
        Fuzz.bool


id : Fuzzer String
id =
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



-- selectStatement : Fuzzer SQLite.Statement.Select.Statement
-- selectStatement =
--     Fuzz.oneOf
--         [Fuzz.map SQLite.Statement.Select.Statement never
--         ]
