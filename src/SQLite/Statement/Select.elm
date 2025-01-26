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


type alias SelectCore =
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
    Parser.problem "Statement.Select.parser"
