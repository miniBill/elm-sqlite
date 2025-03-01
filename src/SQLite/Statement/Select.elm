module SQLite.Statement.Select exposing
    ( Statement
    , parser, toRope
    )

{-|

@docs Statement
@docs parser, toRope

-}

import List.NonEmpty
import Parser.OfTokens as Parser exposing (Parser)
import Parser.Token as Token exposing (Token)
import Rope exposing (Rope)
import Rope.Extra
import SQLite.Expr as Expr exposing (Expr)


type alias Statement =
    { commonTableClause : Maybe CommonTableClause
    , selectTree : SelectTree
    , orderBy : List.NonEmpty.NonEmpty OrderingTerm
    , limit : Maybe Limit
    }


type alias CommonTableClause =
    { recursive : Bool
    , commonTableExpressions : List.NonEmpty.NonEmpty CommonTableExpression
    }


type alias CommonTableExpression =
    Never


type SelectTree
    = Leaf SelectCore
    | Union SelectTree SelectTree
    | UnionAll SelectTree SelectTree
    | Intersect SelectTree SelectTree
    | Except SelectTree SelectTree


type SelectCore
    = Select
        { modifier : Maybe Modifier
        , columns : List.NonEmpty.NonEmpty ResultColumn
        , from : Maybe From
        , where_ : Maybe Expr
        , groupBy : Maybe (List.NonEmpty.NonEmpty Expr)
        , having : Maybe Expr
        , window : Maybe (List.NonEmpty.NonEmpty Window)
        }
    | Values (List.NonEmpty.NonEmpty (List.NonEmpty.NonEmpty Expr))


type Modifier
    = Distinct
    | All


type alias ResultColumn =
    Never


type From
    = FromTableOrSubquery TableOrSubquery
    | FromJoinClause JoinClause


type alias TableOrSubquery =
    Never


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
                            |> List.NonEmpty.toList
                            |> List.map commonTableExpressionToRope
                            |> List.intersperse (Rope.singleton ",")
                            |> Rope.Extra.fromListOfRopes
                        )
                    |> Rope.append ")"
            )
            statement.commonTableClause


commonTableExpressionToRope : CommonTableExpression -> Rope String
commonTableExpressionToRope cte =
    never cte


parser : Parser Token Statement
parser =
    Parser.succeed Statement
        |> Parser.oneOf_
            [ Parser.map Just commonTableClauseParser
            , Parser.succeed Nothing
            ]
        |> Parser.keep treeParser
        |> Parser.keep orderByParser
        |> Parser.keep limitParser


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
    Parser.problem "Select.commonTableExpressionParser"


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
        [ Parser.succeed Select
            |> Parser.token_ Token.Select
            |> Parser.keep (Parser.problem "Select.selectCoreParser#SELECT")
        , Parser.succeed Values
            |> Parser.manyWithSeparator_ Token.Comma
                (Parser.succeed identity
                    |> Parser.token_ Token.ParensOpen
                    |> Parser.manyWithSeparator_ Token.Comma Expr.parser
                    |> Parser.token_ Token.ParensClose
                )
        ]


orderByParser : Parser token (List.NonEmpty.NonEmpty OrderingTerm)
orderByParser =
    Parser.problem "Select.orderByParser"


limitParser : Parser token (Maybe Limit)
limitParser =
    Parser.problem "Select.limitParser"
