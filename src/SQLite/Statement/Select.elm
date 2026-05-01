module SQLite.Statement.Select exposing
    ( Statement, StatementBuilder
    , parser, toRope
    , select, select_, from, from_
    , values
    , expr, ResultColumn
    , unionAll
    , with, withRecursive, commonTableExpression
    , where_
    )

{-|

@docs Statement, StatementBuilder
@docs parser, toRope


## Builders


### Main statement structure:

@docs select, select_, from, from_

@docs values


### Select clause

@docs expr, ResultColumn


### Combining subqueries

@docs unionAll


## Common Table Expressions

@docs with, withRecursive, commonTableExpression

-}

import List.NonEmpty as NonEmpty exposing (NonEmpty)
import Parser.Extra as Parser
import Parser.OfTokens as Parser exposing (Node(..), Parser, token_)
import Parser.Token as Token exposing (Token)
import Rope exposing (Rope)
import Rope.Extra
import SQLite.Expr as Expr exposing (Expr)
import SQLite.Types exposing (ColumnName, SchemaName, TableName)


type alias Statement =
    { commonTableClause : Maybe CommonTableClause
    , selectTree : SelectTree
    , orderBy : Maybe OrderBy
    , limit : Maybe Limit
    }


type alias StatementBuilder cte orderBy limit =
    { commonTableClause : Maybe cte
    , selectTree : SelectTree
    , orderBy : Maybe orderBy
    , limit : Maybe limit
    }


type alias OrderBy =
    NonEmpty OrderingTerm


type alias CommonTableClause =
    { recursive : Bool
    , commonTableExpressions : NonEmpty CommonTableExpression
    }


type CommonTableExpression
    = CommonTableExpression
        { tableName : TableName
        , columns : Maybe (NonEmpty ColumnName)
        , materialized : Maybe Bool
        , select : Statement
        }


commonTableExpression : TableName -> List ColumnName -> Statement -> CommonTableExpression
commonTableExpression tableName columns s =
    CommonTableExpression
        { tableName = tableName
        , columns = NonEmpty.fromList columns
        , materialized = Nothing
        , select = s
        }


type SelectTree
    = Leaf SelectCore
    | Union SelectTree SelectTree
    | UnionAll SelectTree SelectTree
    | Intersect SelectTree SelectTree
    | Except SelectTree SelectTree


type SelectCore
    = Select SelectData
    | Values (NonEmpty (NonEmpty Expr))


type alias SelectData =
    { modifier : Maybe Modifier
    , columns : NonEmpty ResultColumn
    , from : Maybe From
    , where_ : Maybe Expr
    , groupBy : Maybe (NonEmpty Expr)
    , having : Maybe Expr
    , window : Maybe (NonEmpty Window)
    }


type Modifier
    = Distinct
    | All


type ResultColumn
    = ResultColumnExpr Expr (Maybe ColumnAlias)
    | ResultColumnStar
    | ResultColumnTableStar TableName


type alias ColumnAlias =
    ColumnName


type From
    = FromTableOrSubquery (NonEmpty TableOrSubquery)
    | FromJoinClause JoinClause


type TableOrSubquery
    = TableOrSubqueryTable
        { schemaName : Maybe SchemaName
        , tableName : TableName
        , alias : Maybe TableAlias
        , indexed : Maybe Indexed
        }
    | TableOrSubqueryFunctionName
        { schemaName : Maybe SchemaName
        , tableFunctionName : TableFunctionName
        , alias : Maybe TableAlias
        , args : NonEmpty Expr
        }
    | TableOrSubquerySelect
        { statement : Statement
        , alias : Maybe TableAlias
        }
    | TableOrSubqueryJoin JoinClause
    | TableOrSubqueryTuple (NonEmpty TableOrSubquery)


type Indexed
    = IndexedBy IndexName
    | NotIndexed


type alias TableFunctionName =
    String


type alias IndexName =
    String


type alias TableAlias =
    String


type alias JoinClause =
    Never


type alias Window =
    { windowName : WindowName
    , windowDefinition : Never
    }


type alias WindowName =
    Never


type alias OrderingTerm =
    { expr : Expr
    , collate : Maybe CollationName
    }


type alias CollationName =
    Never


type Limit
    = Limit Expr
    | LimitOffset Expr Expr
    | LimitComma Expr Expr


toRope : Statement -> Rope String
toRope statement =
    Rope.empty
        |> Rope.Extra.appendMaybe
            (\{ recursive, commonTableExpressions } ->
                Rope.singleton "WITH"
                    |> Rope.Extra.appendIf recursive
                        "RECURSIVE"
                    |> Rope.append "("
                    |> Rope.prependTo
                        (commonTableExpressions
                            |> NonEmpty.toList
                            |> List.map commonTableExpressionToRope
                            |> List.intersperse (Rope.singleton ",")
                            |> Rope.Extra.fromListOfRopes
                        )
                    |> Rope.append ")"
            )
            statement.commonTableClause


commonTableExpressionToRope : CommonTableExpression -> Rope String
commonTableExpressionToRope (CommonTableExpression cte) =
    Rope.singleton cte.tableName
        |> Rope.Extra.appendMaybe
            (\l ->
                Rope.singleton "("
                    |> Rope.append
                        (l
                            |> NonEmpty.toList
                            |> String.join ", "
                        )
                    |> Rope.append ")"
            )
            cte.columns
        |> Rope.append " AS "
        |> Rope.Extra.appendMaybe
            (\b ->
                if b then
                    Rope.singleton "MATERIALIZED "

                else
                    Rope.singleton "NOT MATERIALIZED "
            )
            cte.materialized
        |> Rope.append "("
        |> Rope.prependTo (toRope cte.select)
        |> Rope.append ")"


parser : Parser Token Statement
parser =
    Parser.succeed Statement
        |> Parser.oneOf_
            [ Parser.map Just commonTableClauseParser
            , Parser.succeed Nothing
            ]
        |> Parser.keep treeParser
        |> Parser.maybe_ orderByParser
        |> Parser.maybe_ limitParser


commonTableClauseParser : Parser Token CommonTableClause
commonTableClauseParser =
    Parser.succeed
        (\recursive commonTableExpressions ->
            { recursive = recursive /= Nothing
            , commonTableExpressions = commonTableExpressions
            }
        )
        |> Parser.skip (Parser.token Token.With)
        |> Parser.maybe_ (Parser.token Token.Recursive)
        |> Parser.manyWithSeparator_ Token.Comma commonTableExpressionParser


commonTableExpressionParser : Parser Token CommonTableExpression
commonTableExpressionParser =
    Parser.succeed
        (\tableName columns materialized s ->
            CommonTableExpression
                { tableName = tableName
                , columns = columns
                , materialized = materialized
                , select = s
                }
        )
        |> Parser.ident_
        |> Parser.maybe_
            (Parser.succeed identity
                |> Parser.token_ Token.ParensOpen
                |> Parser.manyWithSeparator_ Token.Comma Parser.ident
                |> Parser.token_ Token.ParensClose
            )
        |> Parser.token_ Token.As
        |> Parser.oneOf_
            [ Parser.succeed (Just False)
                |> Parser.token_ Token.Not
                |> Parser.token_ Token.Materialized
            , Parser.succeed (Just True)
                |> Parser.token_ Token.Materialized
            , Parser.succeed Nothing
            ]
        |> Parser.token_ Token.ParensOpen
        |> Parser.keep (Parser.lazy (\_ -> parser))
        |> Parser.token_ Token.ParensClose


treeParser : Parser Token SelectTree
treeParser =
    let
        leafParser : Parser Token SelectTree
        leafParser =
            Parser.map Leaf selectCoreParser
    in
    Parser.succeed (List.foldl identity)
        |> Parser.keep leafParser
        |> Parser.many_
            (Parser.succeed (\op r l -> op l r)
                |> Parser.keep treeOperatorParser
                |> Parser.keep leafParser
            )


treeOperatorParser : Parser Token (SelectTree -> SelectTree -> SelectTree)
treeOperatorParser =
    Parser.oneOf
        [ Parser.succeed
            (\all ->
                if all == Nothing then
                    Union

                else
                    UnionAll
            )
            |> Parser.token_ Token.Union
            |> Parser.maybe_ (Parser.token Token.All)
        , Parser.succeed Intersect
            |> Parser.token_ Token.Intersect
        , Parser.succeed Except
            |> Parser.token_ Token.Except
        ]


selectCoreParser : Parser Token SelectCore
selectCoreParser =
    Parser.oneOf
        [ Parser.succeed SelectData
            |> Parser.token_ Token.Select
            |> Parser.oneOf_
                [ Parser.succeed (Just Distinct) |> Parser.token_ Token.Distinct
                , Parser.succeed (Just All) |> Parser.token_ Token.All
                , Parser.succeed Nothing
                ]
            |> Parser.manyWithSeparator_ Token.Comma resultColumnParser
            |> Parser.maybe_
                (Parser.succeed identity
                    |> Parser.token_ Token.From
                    |> Parser.oneOf_
                        [ Parser.succeed FromTableOrSubquery
                            |> Parser.manyWithSeparator_ Token.Comma tableOrSubqueryParser
                        , Parser.succeed FromJoinClause
                            |> Parser.keep joinClauseParser
                        ]
                )
            |> Parser.maybe_
                (Parser.succeed identity
                    |> Parser.token_ Token.Where
                    |> Parser.keep Expr.parser
                )
            |> Parser.maybe_
                (Parser.succeed identity
                    |> Parser.token_ Token.Group
                    |> Parser.token_ Token.By
                    |> Parser.manyWithSeparator_ Token.Comma Expr.parser
                )
            |> Parser.maybe_
                (Parser.succeed identity
                    |> Parser.token_ Token.Having
                    |> Parser.keep Expr.parser
                )
            |> Parser.maybe_
                (Parser.succeed identity
                    |> Parser.token_ Token.Window
                    |> Parser.manyWithSeparator_ Token.Comma windowParser
                )
            |> Parser.map Select
        , Parser.succeed Values
            |> Parser.token_ Token.Values
            |> Parser.manyWithSeparator_ Token.Comma
                (Parser.succeed identity
                    |> Parser.token_ Token.ParensOpen
                    |> Parser.manyWithSeparator_ Token.Comma Expr.parser
                    |> Parser.token_ Token.ParensClose
                )
        ]


windowParser : Parser Token Window
windowParser =
    Parser.succeed Window
        |> Parser.keep (Parser.problem "window-name")
        |> Parser.token_ Token.As
        |> Parser.keep (Parser.problem "window-defn")


tableOrSubqueryParser : Parser Token TableOrSubquery
tableOrSubqueryParser =
    Parser.oneOf
        [ Parser.succeed
            (\schemaName tableName alias indexed ->
                TableOrSubqueryTable
                    { schemaName = schemaName
                    , tableName = tableName
                    , alias = alias
                    , indexed = indexed
                    }
            )
            |> Parser.maybe_
                (Parser.succeed identity
                    |> Parser.backtrackable_ Parser.ident
                    |> Parser.token_ Token.Dot
                )
            |> Parser.ident_
            |> Parser.oneOf_
                [ Parser.succeed Just
                    |> Parser.token_ Token.As
                    |> Parser.ident_
                , Parser.succeed Just
                    |> Parser.ident_
                , Parser.succeed Nothing
                ]
            |> Parser.oneOf_
                [ Parser.succeed (\n -> Just (IndexedBy n))
                    |> Parser.token_ Token.Indexed
                    |> Parser.token_ Token.By
                    |> Parser.ident_
                , Parser.succeed (Just NotIndexed)
                    |> Parser.token_ Token.Not
                    |> Parser.token_ Token.Indexed
                , Parser.succeed Nothing
                ]
        , Parser.succeed TableOrSubqueryFunctionName
            |> Parser.keep (Parser.problem "Select.tableOrSubqueryParser.TableOrSubqueryFunctionName")
        , Parser.succeed TableOrSubquerySelect
            |> Parser.keep (Parser.problem "Select.tableOrSubqueryParser.TableOrSubquerySelect")
        , Parser.succeed TableOrSubqueryJoin
            |> Parser.keep (Parser.problem "Select.tableOrSubqueryParser.TableOrSubqueryJoin")
        , Parser.succeed TableOrSubqueryTuple
            |> Parser.keep (Parser.problem "Select.tableOrSubqueryParser.TableOrSubqueryTuple")
        ]


joinClauseParser : Parser Token JoinClause
joinClauseParser =
    Parser.problem "join-clause"


resultColumnParser : Parser Token ResultColumn
resultColumnParser =
    Parser.oneOf
        [ Parser.succeed ResultColumnStar |> Parser.token_ Token.Star
        , Parser.custom
            (\position stream ->
                case stream of
                    (Node _ (Token.Ident tableName)) :: (Node _ Token.Dot) :: (Node tableRange Token.Star) :: tail ->
                        Parser.Good True (ResultColumnTableStar tableName) tableRange.end tail

                    _ ->
                        Parser.errorAt False position (Parser.Problem "Expecting table name")
            )
        , Parser.succeed (\e _ c -> ResultColumnExpr e c)
            |> Parser.keep Expr.parser
            |> Parser.maybe_ (Parser.token Token.As)
            |> Parser.maybe_ Parser.ident
        ]


orderByParser : Parser token (NonEmpty OrderingTerm)
orderByParser =
    Parser.problem "Select.orderByParser"


limitParser : Parser token Limit
limitParser =
    Parser.problem "Select.limitParser"


type Selecting
    = Selecting Never


with :
    NonEmpty CommonTableExpression
    -> StatementBuilder Never order limit
    -> StatementBuilder CommonTableClause order limit
with exprs builder =
    { commonTableClause =
        Just
            { recursive = False
            , commonTableExpressions = exprs
            }
    , selectTree = builder.selectTree
    , orderBy = builder.orderBy
    , limit = builder.limit
    }


withRecursive :
    NonEmpty CommonTableExpression
    -> StatementBuilder Never order limit
    -> StatementBuilder CommonTableClause order limit
withRecursive exprs builder =
    { commonTableClause =
        Just
            { recursive = True
            , commonTableExpressions = exprs
            }
    , selectTree = builder.selectTree
    , orderBy = builder.orderBy
    , limit = builder.limit
    }


select : NonEmpty ColumnName -> StatementBuilder cte order limit
select columns =
    select_ (NonEmpty.map (\n -> ResultColumnExpr (Expr.columnName n) Nothing) columns)


select_ : NonEmpty ResultColumn -> StatementBuilder cte order limit
select_ columns =
    let
        core : SelectCore
        core =
            Select
                { modifier = Nothing
                , columns = columns
                , from = Nothing
                , where_ = Nothing
                , groupBy = Nothing
                , having = Nothing
                , window = Nothing
                }
    in
    { commonTableClause = Nothing
    , selectTree = Leaf core
    , orderBy = Nothing
    , limit = Nothing
    }


from : NonEmpty TableName -> StatementBuilder Never Never Never -> StatementBuilder cte order limit
from f stat =
    from_
        (FromTableOrSubquery
            (NonEmpty.map
                (\t ->
                    TableOrSubqueryTable
                        { schemaName = Nothing
                        , tableName = t
                        , alias = Nothing
                        , indexed = Nothing
                        }
                )
                f
            )
        )
        stat


from_ : From -> StatementBuilder Never Never Never -> StatementBuilder cte order limit
from_ f stat =
    let
        applyFrom : SelectTree -> SelectTree
        applyFrom node =
            case node of
                Leaf (Select l) ->
                    Leaf (Select { l | from = Just f })

                Leaf (Values _) ->
                    Debug.todo "branch 'Leaf (Values _)' not implemented"

                Union _ _ ->
                    Debug.todo "branch 'Union _ _' not implemented"

                UnionAll _ _ ->
                    Debug.todo "branch 'UnionAll _ _' not implemented"

                Intersect _ _ ->
                    Debug.todo "branch 'Intersect _ _' not implemented"

                Except _ _ ->
                    Debug.todo "branch 'Except _ _' not implemented"
    in
    { commonTableClause = Nothing
    , selectTree = applyFrom stat.selectTree
    , orderBy = Nothing
    , limit = Nothing
    }


unionAll :
    StatementBuilder Never Never Never
    -> StatementBuilder Never Never Never
    -> StatementBuilder cte order limit
unionAll l r =
    { commonTableClause = Nothing
    , selectTree = UnionAll l.selectTree r.selectTree
    , orderBy = Nothing
    , limit = Nothing
    }


values : NonEmpty (NonEmpty Expr) -> StatementBuilder cte order limit
values tuples =
    { commonTableClause = Nothing
    , selectTree = Leaf (Values tuples)
    , orderBy = Nothing
    , limit = Nothing
    }


expr : Expr -> ResultColumn
expr e =
    ResultColumnExpr e Nothing


where_ : Expr -> StatementBuilder Never Never Never -> StatementBuilder cte order limit
where_ e s =
    let
        applyWhere node =
            case node of
                Leaf (Select sel) ->
                    Leaf (Select { sel | where_ = Just e })

                Leaf (Values _) ->
                    Debug.todo "branch 'Leaf (Values _)' not implemented"

                Union _ _ ->
                    Debug.todo "branch 'Union _ _' not implemented"

                UnionAll _ _ ->
                    Debug.todo "branch 'UnionAll _ _' not implemented"

                Intersect _ _ ->
                    Debug.todo "branch 'Intersect _ _' not implemented"

                Except _ _ ->
                    Debug.todo "branch 'Except _ _' not implemented"
    in
    { commonTableClause = Nothing
    , selectTree = applyWhere s.selectTree
    , orderBy = Nothing
    , limit = Nothing
    }
