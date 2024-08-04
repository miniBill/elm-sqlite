module SQLite.Statement.CreateTable exposing (ColumnConstraint, ColumnDefinition, ForeignKeyClause, GeneratedColumnStorage(..), IndexedColumn, InnerColumnConstraint(..), InnerTableConstraint(..), NameOrExpr(..), OnDeleteUpdate(..), Statement, TableConstraint, TableDefinition(..), TableOptions, parser, toRope)

{-|

@docs ColumnConstraint, ColumnDefinition, ForeignKeyClause, GeneratedColumnStorage, IndexedColumn, InnerColumnConstraint, InnerTableConstraint, NameOrExpr, OnDeleteUpdate, Statement, TableConstraint, TableDefinition, TableOptions, parser, toRope

-}

import Parser.OfTokens as Parser exposing (Node(..), Parser)
import Parser.Token as Token exposing (Token)
import Rope exposing (Rope)
import Rope.Extra
import SQLite.Expr as Expr exposing (Expr)
import SQLite.Statement.Select as Select
import SQLite.Types as Types exposing (AscDesc, ConflictClause(..), Type)


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


maybeToString : (a -> b) -> Maybe a -> Rope b
maybeToString f val =
    case val of
        Nothing ->
            Rope.empty

        Just v ->
            Rope.singleton (f v)


maybe : (a -> Rope b) -> Maybe a -> Rope b
maybe f val =
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
            (maybeToString Types.typeToString def.tipe)
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
            Rope.singleton "PRIMARY KEY"
                |> Rope.prependTo (maybeToString Types.ascDescToString ascDesc)
                |> Rope.prependTo (maybe conflictClauseToRope conflictClause)
                |> Rope.prependTo (iif autoIncrement "AUTOINCREMENT")

        ColumnNotNull conflictClause ->
            Rope.singleton "NOT NULL"
                |> Rope.prependTo (maybe conflictClauseToRope conflictClause)

        ColumnUnique conflictClause ->
            Rope.singleton "UNIQUE"
                |> Rope.prependTo (maybe conflictClauseToRope conflictClause)

        ColumnCheck expr ->
            Rope.singleton "CHECK"
                |> Rope.append "("
                |> Rope.prependTo (Expr.toRope expr)
                |> Rope.append ")"

        ColumnDefault (Expr.LiteralValue literal) ->
            Rope.singleton "DEFAULT"
                |> Rope.append (Expr.literalValueToString literal)

        ColumnDefault value ->
            Rope.singleton "DEFAULT"
                |> Rope.append "("
                |> Rope.prependTo (Expr.toRope value)
                |> Rope.append ")"

        ColumnCollate name ->
            Rope.singleton "COLLATE"
                |> Rope.append name

        ColumnForeignKey foreignKeyClause ->
            foreignKeyClauseToRope foreignKeyClause

        GeneratedAs { always } expr storage ->
            (if always then
                Rope.singleton "GENERATED ALWAYS"

             else
                Rope.empty
            )
                |> Rope.append "AS"
                |> Rope.append "("
                |> Rope.prependTo (Expr.toRope expr)
                |> Rope.append ")"
                |> appendMaybe storageToString storage


storageToString : GeneratedColumnStorage -> Rope String
storageToString storage =
    case storage of
        Stored ->
            Rope.singleton "STORED"

        Virtual ->
            Rope.singleton "VIRTUAL"


conflictClauseToRope : ConflictClause -> Rope String
conflictClauseToRope clause =
    Rope.singleton "ON CONFLICT"
        |> Rope.append
            (case clause of
                Rollback ->
                    "ROLLBACK"

                Abort ->
                    "ABORT"

                Fail ->
                    "FAIL"

                Ignore ->
                    "IGNORE"

                Replace ->
                    "REPLACE"
            )


tableConstraintToRope : TableConstraint -> Rope String
tableConstraintToRope constraint =
    case constraint.name of
        Just name ->
            Rope.singleton "CONSTRAINT"
                |> Rope.append name
                |> Rope.prependTo
                    (innerTableConstraintToRope constraint.constraint)

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
                |> Rope.prependTo (maybe conflictClauseToRope conflictClause)

        TableUnique columns conflictClause ->
            Rope.singleton "UNIQUE"
                |> Rope.append "("
                |> Rope.prependTo
                    (columns
                        |> List.map indexedColumnToRope
                        |> List.intersperse (Rope.singleton ",")
                        |> fromTokens
                    )
                |> Rope.append ")"
                |> Rope.prependTo (maybe conflictClauseToRope conflictClause)

        TableCheck expr ->
            Rope.singleton "CHECK"
                |> Rope.append "("
                |> Rope.prependTo (Expr.toRope expr)
                |> Rope.append ")"

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
        |> appendMaybe
            (\onDelete ->
                Rope.singleton "ON DELETE"
                    |> Rope.append (onDeleteUpdateToString onDelete)
            )
            clause.onDelete
        |> appendMaybe
            (\onUpdate ->
                Rope.singleton "ON UPDATE"
                    |> Rope.append (onDeleteUpdateToString onUpdate)
            )
            clause.onUpdate
        |> appendMaybe
            (\name ->
                Rope.singleton "MATCH"
                    |> Rope.append name
            )
            clause.match
        |> (case clause.defer of
                Nothing ->
                    identity

                Just ever ->
                    never ever
           )


appendMaybe : (a -> Rope b) -> Maybe a -> Rope b -> Rope b
appendMaybe f tail head =
    head
        |> Rope.prependTo (maybe f tail)


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
        |> Rope.prependTo (maybeToString Types.ascDescToString column.ascDesc)


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


onDeleteUpdateToString : OnDeleteUpdate -> String
onDeleteUpdateToString onDeleteUpdate =
    case onDeleteUpdate of
        SetNull ->
            "SET NULL"

        SetDefault ->
            "SET DEFAULT"

        Cascade ->
            "CASCADE"

        Restrict ->
            "RESTRICT"

        NoAction ->
            "NO ACTION"


type alias TableOptions =
    { withoutRowid : Bool
    , strict : Bool
    }


parser : Parser Token Statement
parser =
    Parser.succeed
        (\temporary ifNotExists ( schemaName, name ) definition ->
            { temporary = temporary
            , ifNotExists = ifNotExists
            , schemaName = schemaName
            , name = name
            , definition = definition
            }
        )
        |> Parser.token_ Token.Create
        |> Parser.oneOf_
            [ Parser.succeed True
                |> Parser.skip
                    (Parser.oneOf
                        [ Parser.token Token.Temporary
                        , Parser.token Token.Temp
                        ]
                    )
            , Parser.succeed False
            ]
        |> Parser.token_ Token.Table
        |> Parser.oneOf_
            [ Parser.succeed True
                |> Parser.token_ Token.If
                |> Parser.token_ Token.Not
                |> Parser.token_ Token.Exists
            , Parser.succeed False
            ]
        |> Parser.custom_
            (\position stream ->
                case stream of
                    (Node _ (Token.Ident schema)) :: (Node _ Token.Dot) :: (Node tableRange (Token.Ident table)) :: tail ->
                        Parser.Good True ( Just schema, table ) tableRange.end tail

                    (Node tableRange (Token.Ident table)) :: tail ->
                        Parser.Good True ( Nothing, table ) tableRange.end tail

                    _ ->
                        Parser.errorAt False position (Parser.Problem "Expecting table name")
            )
        |> Parser.keep definitionParser


definitionParser : Parser Token TableDefinition
definitionParser =
    Parser.oneOf
        [ Parser.succeed TableDefinitionSelect
            |> Parser.token_ Token.As
            |> Parser.keep Select.parser
        , Parser.succeed
            (\columnsAndConstraints options ->
                TableDefinitionColumns
                    { columns =
                        List.filterMap
                            (\corc ->
                                case corc of
                                    Ok def ->
                                        Just def

                                    Err _ ->
                                        Nothing
                            )
                            columnsAndConstraints
                    , constraints =
                        List.filterMap
                            (\corc ->
                                case corc of
                                    Ok _ ->
                                        Nothing

                                    Err con ->
                                        Just con
                            )
                            columnsAndConstraints
                    , options = options
                    }
            )
            |> Parser.sequence_
                { start = Token.ParensOpen
                , end = Token.ParensClose
                , separator = Token.Comma
                , item =
                    Parser.oneOf
                        [ Parser.map Err tableConstraintParser
                        , Parser.map Ok columnDefinitionParser
                        ]
                , trailing = Parser.Forbidden
                }
            |> Parser.keep tableOptionsParser
        ]


columnDefinitionParser : Parser Token ColumnDefinition
columnDefinitionParser =
    Parser.succeed ColumnDefinition
        |> Parser.keep ident
        |> Parser.maybe_ Types.typeParser
        |> Parser.many_ columnConstraintParser


columnConstraintParser : Parser Token ColumnConstraint
columnConstraintParser =
    Parser.problem "CreateTable.columnConstraintParser"


tableConstraintParser : Parser Token TableConstraint
tableConstraintParser =
    Parser.succeed TableConstraint
        |> Parser.maybe_
            (Parser.succeed identity
                |> Parser.token_ Token.Constraint
                |> Parser.keep ident
            )
        |> Parser.oneOf_
            [ Parser.succeed TablePrimaryKey
                |> Parser.token_ Token.Primary
                |> Parser.token_ Token.Key
                |> Parser.sequence_
                    { start = Token.ParensOpen
                    , end = Token.ParensClose
                    , separator = Token.Comma
                    , item = indexedColumnParser
                    , trailing = Parser.Forbidden
                    }
                |> Parser.maybe_ conflictClauseParser
            , Parser.succeed TableUnique
                |> Parser.token_ Token.Unique
                |> Parser.sequence_
                    { start = Token.ParensOpen
                    , end = Token.ParensClose
                    , separator = Token.Comma
                    , item = indexedColumnParser
                    , trailing = Parser.Forbidden
                    }
                |> Parser.maybe_ conflictClauseParser
            , Parser.succeed TableCheck
                |> Parser.token_ Token.Check
                |> Parser.token_ Token.ParensOpen
                |> Parser.keep Expr.parser
                |> Parser.token_ Token.ParensClose
            , Parser.succeed TableForeignKey
                |> Parser.sequence_
                    { start = Token.ParensOpen
                    , end = Token.ParensClose
                    , separator = Token.Comma
                    , item = ident
                    , trailing = Parser.Forbidden
                    }
                |> Parser.keep foreignKeyClauseParser
            ]


foreignKeyClauseParser : Parser Token ForeignKeyClause
foreignKeyClauseParser =
    Parser.problem "CreateTable.foreignKeyClauseParser"


conflictClauseParser : Parser Token ConflictClause
conflictClauseParser =
    Parser.succeed identity
        |> Parser.token_ Token.On
        |> Parser.token_ Token.Conflict
        |> Parser.oneOf_
            [ Parser.succeed Rollback |> Parser.token_ Token.Rollback
            , Parser.succeed Abort |> Parser.token_ Token.Abort
            , Parser.succeed Fail |> Parser.token_ Token.Fail
            , Parser.succeed Ignore |> Parser.token_ Token.Ignore
            , Parser.succeed Replace |> Parser.token_ Token.Replace
            ]


indexedColumnParser : Parser Token IndexedColumn
indexedColumnParser =
    Parser.succeed IndexedColumn
        |> Parser.keep nameOrExprParser
        |> Parser.maybe_
            (Parser.succeed identity
                |> Parser.token_ Token.Collate
                |> Parser.keep ident
            )
        |> Parser.maybe_ ascDescParser


ascDescParser : Parser Token AscDesc
ascDescParser =
    Parser.oneOf
        [ Parser.succeed Types.Asc |> Parser.token_ Token.Asc
        , Parser.succeed Types.Desc |> Parser.token_ Token.Desc
        ]


nameOrExprParser : Parser Token NameOrExpr
nameOrExprParser =
    Parser.oneOf
        [ Expr.parser
            |> Parser.map
                (\expr ->
                    let
                        _ =
                            Debug.todo
                    in
                    -- case expr of
                    --     Expr.ColumnName Nothing Nothing name ->
                    --         IsName name
                    --     _ ->
                    IsExpr expr
                )
        , Parser.map IsName ident
        ]


ident : Parser Token String
ident =
    Parser.custom
        (\position stream ->
            case stream of
                (Node identRange (Token.Ident name)) :: tail ->
                    Parser.Good True name identRange.end tail

                _ ->
                    Parser.errorAt False position (Parser.Problem "Expecting identifier")
        )


tableOptionsParser : Parser Token TableOptions
tableOptionsParser =
    let
        inner : Parser Token TableOptions
        inner =
            Parser.oneOf
                [ Parser.succeed { strict = False, withoutRowid = True }
                    |> Parser.token_ Token.Without
                    |> Parser.token_ (Token.Ident "ROWID")
                , Parser.succeed { strict = True, withoutRowid = False }
                    |> Parser.token_ (Token.Ident "STRICT")
                ]

        default : TableOptions
        default =
            { strict = False, withoutRowid = False }

        combine : TableOptions -> TableOptions -> TableOptions
        combine l r =
            { strict = l.strict || r.strict
            , withoutRowid = l.withoutRowid || r.withoutRowid
            }
    in
    Parser.oneOf
        [ Parser.succeed combine
            |> Parser.keep inner
            |> Parser.oneOf_
                [ Parser.succeed identity
                    |> Parser.token_ Token.Comma
                    |> Parser.keep inner
                , Parser.succeed default
                ]
        , Parser.succeed default
        ]
