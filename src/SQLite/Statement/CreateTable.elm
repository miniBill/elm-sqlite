module SQLite.Statement.CreateTable exposing (ColumnConstraint, ColumnDefinition, ConflictClause(..), ForeignKeyClause, GeneratedColumnStorage(..), IndexedColumn, InnerColumnConstraint(..), InnerTableConstraint(..), NameOrExpr(..), OnDeleteUpdate(..), Statement, TableConstraint, TableDefinition(..), TableOptions, toRope)

import Rope exposing (Rope)
import Rope.Extra
import SQLite.Expr as Expr exposing (Expr)
import SQLite.Statement.Select as Select
import SQLite.Types as Types exposing (AscDesc, Type)


type alias Statement =
    { temporary : Bool
    , ifNotExists : Bool
    , schemaName : Maybe String
    , name : String
    , definition : TableDefinition
    }


toRope : Statement -> Rope String
toRope statement =
    Rope.empty
        |> Rope.append "CREATE"
        |> Rope.prependTo (iif statement.temporary "TEMP")
        |> Rope.append "TABLE"
        |> Rope.prependTo (iif statement.ifNotExists "IF NOT EXISTS")
        |> Rope.prependTo
            (case statement.schemaName of
                Nothing ->
                    Rope.singleton statement.name

                Just sn ->
                    Rope.singleton (sn ++ "." ++ statement.name)
            )
        |> Rope.prependTo
            (case statement.definition of
                TableDefinitionSelect select ->
                    Rope.prepend "AS" (Select.toRope select)

                TableDefinitionColumns columnsDef ->
                    columnsDefToRope columnsDef
            )


fromTokens : List (Rope String) -> Rope String
fromTokens list =
    list
        |> Rope.fromList
        |> Rope.concat


iif : Bool -> String -> Rope String
iif condition text =
    if condition then
        Rope.singleton text

    else
        Rope.empty


maybe : (a -> b) -> Maybe a -> Rope b
maybe f val =
    case val of
        Nothing ->
            Rope.empty

        Just v ->
            Rope.singleton (f v)


maybe_ : (a -> Rope b) -> Maybe a -> Rope b
maybe_ f val =
    case val of
        Nothing ->
            Rope.empty

        Just v ->
            f v


type TableDefinition
    = TableDefinitionColumns
        { columns : List ColumnDefinition
        , constraints : List TableConstraint
        , options : TableOptions
        }
    | TableDefinitionSelect Select.Statement


columnsDefToRope :
    { columns : List ColumnDefinition
    , constraints : List TableConstraint
    , options : TableOptions
    }
    -> Rope String
columnsDefToRope def =
    Rope.empty
        |> Rope.append "("
        |> Rope.prependTo
            ((List.map columnDefinitionToRope def.columns
                ++ List.map tableConstraintToRope def.constraints
             )
                |> List.intersperse (Rope.singleton ",")
                |> fromTokens
            )
        |> Rope.append ")"
        |> Rope.prependTo (tableOptionsToRope def.options)


columnDefinitionToRope : ColumnDefinition -> Rope String
columnDefinitionToRope def =
    Rope.prepend def.name
        (Rope.appendTo
            (maybe Types.typeToString def.tipe)
            (Rope.concatMap
                columnConstraintToRope
                (Rope.fromList def.constraints)
            )
        )


columnConstraintToRope : ColumnConstraint -> Rope String
columnConstraintToRope constraint =
    case constraint.name of
        Just name ->
            Rope.prepend "CONSTRAINT" (Rope.prepend name (innerColumnConstraintToRope constraint.constraint))

        Nothing ->
            innerColumnConstraintToRope constraint.constraint


innerColumnConstraintToRope : InnerColumnConstraint -> Rope String
innerColumnConstraintToRope constraint =
    case constraint of
        ColumnPrimaryKey ascDesc conflictClause { autoIncrement } ->
            [ Rope.singleton "PRIMARY KEY"
            , maybe Types.ascDescToString ascDesc
            , maybe_ conflictClauseToRope conflictClause
            , iif autoIncrement "AUTOINCREMENT"
            ]
                |> fromTokens

        ColumnNotNull conflictClause ->
            [ Rope.singleton "NOT NULL"
            , maybe_ conflictClauseToRope conflictClause
            ]
                |> fromTokens

        ColumnUnique _ ->
            Debug.todo "innerColumnConstraintToRope - branch 'ColumnUnique _' not implemented"

        ColumnCheck _ ->
            Debug.todo "innerColumnConstraintToRope - branch 'ColumnCheck _' not implemented"

        ColumnDefault _ ->
            Debug.todo "innerColumnConstraintToRope - branch 'ColumnDefault _' not implemented"

        ColumnCollate _ ->
            Debug.todo "innerColumnConstraintToRope - branch 'ColumnCollate _' not implemented"

        ColumnForeignKey foreignKeyClause ->
            foreignKeyClauseToRope foreignKeyClause

        GeneratedAs _ _ _ ->
            Debug.todo "innerColumnConstraintToRope - branch 'GeneratedAs _ _ _' not implemented"


conflictClauseToRope : ConflictClause -> Rope String
conflictClauseToRope _ =
    Debug.todo "conflictClauseToRope"


tableConstraintToRope : TableConstraint -> Rope String
tableConstraintToRope constraint =
    case constraint.name of
        Just name ->
            Rope.prepend "CONSTRAINT" (Rope.prepend name (innerTableConstraintToRope constraint.constraint))

        Nothing ->
            innerTableConstraintToRope constraint.constraint


innerTableConstraintToRope : InnerTableConstraint -> Rope String
innerTableConstraintToRope constraint =
    case constraint of
        TablePrimaryKey columns conflictClause ->
            Rope.singleton "PRIMARY KEY"
                |> Rope.append "("
                |> Rope.prependTo
                    (columns
                        |> List.map indexedColumnToRope
                        |> List.intersperse (Rope.singleton ",")
                        |> fromTokens
                    )
                |> Rope.append ")"
                |> Rope.prependTo (maybe_ conflictClauseToRope conflictClause)

        TableUnique _ _ ->
            Debug.todo "innerTableConstraintToRope - branch 'TableUnique _ _' not implemented"

        TableCheck _ ->
            Debug.todo "innerTableConstraintToRope - branch 'TableCheck _' not implemented"

        TableForeignKey columns foreignKeyClause ->
            Rope.singleton "FOREIGN KEY"
                |> appendColumnList columns
                |> Rope.prependTo (foreignKeyClauseToRope foreignKeyClause)


foreignKeyClauseToRope : ForeignKeyClause -> Rope String
foreignKeyClauseToRope clause =
    Rope.singleton "REFERENCES"
        |> Rope.append clause.foreignTable
        |> (if List.isEmpty clause.columnNames then
                identity

            else
                appendColumnList clause.columnNames
           )
        |> (case clause.onDelete of
                Nothing ->
                    identity

                Just _ ->
                    Debug.todo "foreignKeyClauseToRope - onDelete - branch 'Just _' not implemented"
           )
        |> (case clause.onUpdate of
                Nothing ->
                    identity

                Just _ ->
                    Debug.todo "foreignKeyClauseToRope - onUpdate - branch 'Just _' not implemented"
           )
        |> (case clause.match of
                Nothing ->
                    identity

                Just _ ->
                    Debug.todo "foreignKeyClauseToRope - match - branch 'Just _' not implemented"
           )
        |> (case clause.defer of
                Nothing ->
                    identity

                Just ever ->
                    never ever
           )


appendColumnList : List String -> Rope String -> Rope String
appendColumnList list rope =
    rope
        |> Rope.append "("
        |> Rope.prependTo
            (list
                |> List.intersperse ","
                |> Rope.fromList
            )
        |> Rope.append ")"


indexedColumnToRope : IndexedColumn -> Rope String
indexedColumnToRope column =
    Rope.empty
        |> Rope.prependTo
            (case column.nameOrExpr of
                IsName name ->
                    Rope.singleton name

                IsExpr expr ->
                    Expr.toRope expr
            )
        |> Rope.prependTo
            (case column.collate of
                Nothing ->
                    Rope.empty

                Just name ->
                    Rope.singleton ("COLLATE " ++ name)
            )
        |> Rope.prependTo (maybe Types.ascDescToString column.ascDesc)


tableOptionsToRope : TableOptions -> Rope String
tableOptionsToRope options =
    [ iif options.strict "STRICT"
    , iif options.withoutRowid "WITHOUT ROWID"
    ]
        |> fromTokens
        |> Rope.Extra.intersperse ","


type alias ColumnDefinition =
    { name : String
    , tipe : Maybe Type
    , constraints : List ColumnConstraint
    }


type alias ColumnConstraint =
    { name : Maybe String
    , constraint : InnerColumnConstraint
    }


type InnerColumnConstraint
    = ColumnPrimaryKey (Maybe AscDesc) (Maybe ConflictClause) { autoIncrement : Bool }
    | ColumnNotNull (Maybe ConflictClause)
    | ColumnUnique (Maybe ConflictClause)
    | ColumnCheck Expr
    | ColumnDefault Expr
    | ColumnCollate String
    | ColumnForeignKey ForeignKeyClause
    | GeneratedAs { always : Bool } Expr (Maybe GeneratedColumnStorage)


type GeneratedColumnStorage
    = Stored
    | Virtual


type alias TableConstraint =
    { name : Maybe String
    , constraint : InnerTableConstraint
    }


type InnerTableConstraint
    = TablePrimaryKey (List IndexedColumn) (Maybe ConflictClause)
    | TableUnique (List IndexedColumn) (Maybe ConflictClause)
    | TableCheck Expr
    | TableForeignKey (List String) ForeignKeyClause


type alias IndexedColumn =
    { nameOrExpr : NameOrExpr
    , collate : Maybe String
    , ascDesc : Maybe AscDesc
    }


type NameOrExpr
    = IsName String
    | IsExpr Expr


type ConflictClause
    = OnConflictRollback
    | OnConflictAbort
    | OnConflictFail
    | OnConflictIgnore
    | OnConflictReplace


type alias ForeignKeyClause =
    { foreignTable : String
    , columnNames : List String
    , onDelete : Maybe OnDeleteUpdate
    , onUpdate : Maybe OnDeleteUpdate
    , match : Maybe String
    , defer : Maybe Never
    }


type OnDeleteUpdate
    = SetNull
    | SetDefault
    | Cascade
    | Restrict
    | NoAction


type alias TableOptions =
    { withoutRowid : Bool
    , strict : Bool
    }
