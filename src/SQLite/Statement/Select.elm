module SQLite.Statement.Select exposing
    ( Statement
    , parser, toRope
    )

{-|

@docs Statement
@docs parser, toRope

-}

import List.Extra
import List.NonEmpty
import Parser.OfTokens as Parser exposing (Parser)
import Rope exposing (Rope)
import Rope.Extra
import SQLite.Expr exposing (Expr)


type alias Statement =
    { commonTableExpression :
        Maybe
            { recursive : Bool
            , commonTableExpressions : List.NonEmpty.NonEmpty CommonTableExpression
            }
    , selectTree : SelectTree
    , orderBy : List.NonEmpty.NonEmpty OrderingTerm
    , limit : Maybe Limit
    }


type alias CommonTableExpression =
    Never


type SelectTree
    = Leaf SelectCore
    | Union SelectTree SelectTree
    | UnionAll SelectTree SelectTree
    | Insersect SelectTree SelectTree
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
    | Values (List.NonEmpty.NonEmpty Expr)


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
            statement.commonTableExpression


commonTableExpressionToRope : CommonTableExpression -> Rope String
commonTableExpressionToRope cte =
    never cte


parser : Parser token Statement
parser =
    Parser.succeed Statement
        |> Parser.keep commonTableExpressionParser
        |> Parser.keep treeParser
        |> Parser.keep orderByParser
        |> Parser.keep limitParser


commonTableExpressionParser : Parser token (Maybe { recursive : Bool, commonTableExpressions : List.NonEmpty.NonEmpty CommonTableExpression })
commonTableExpressionParser =
    Parser.problem "Select.commonTableExpressionParser"


treeParser : Parser token SelectTree
treeParser =
    Parser.problem "Select.treeParser"


orderByParser : Parser token (List.NonEmpty.NonEmpty OrderingTerm)
orderByParser =
    Parser.problem "Select.orderByParser"


limitParser : Parser token (Maybe Limit)
limitParser =
    Parser.problem "Select.limitParser"
