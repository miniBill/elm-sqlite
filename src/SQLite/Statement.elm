module SQLite.Statement exposing (Explain(..), InnerStatement(..), Statement, parser, toString)

import Parser exposing ((|.), (|=), Parser)
import Parser.Extra as Parser
import Rope exposing (Rope)
import SQLite.Statement.CreateTable as CreateTable
import SQLite.Statement.Select as Select
import SQLite.Types as Types


type alias Statement =
    { explain : Maybe Explain
    , inner : InnerStatement
    }


type Explain
    = Explain
    | ExplainQueryPlan


type InnerStatement
    = AlterTable Never
    | Analyze Never
    | Attach Never
    | Begin Never
    | Commit Never
    | CreateIndex Never
    | CreateTable CreateTable.Statement
    | CreateTrigger Never
    | CreateView Never
    | CreateVirtualTable Never
    | Delete Never
    | DeleteLimited Never
    | Detach Never
    | DropIndex Never
    | DropTable Never
    | DropTrigger Never
    | DropView Never
    | Insert Never
    | Pragma Never
    | Reindex Never
    | Release Never
    | Rollback Never
    | Savepoint Never
    | Select Select.Statement
    | Update Never
    | UpdateLimited Never
    | Vacuum Never


parser : Parser Statement
parser =
    Parser.succeed Statement
        |= Parser.oneOf
            [ Parser.succeed Just
                |. Parser.symbol_ "EXPLAIN"
                |= Parser.oneOf
                    [ Parser.succeed ExplainQueryPlan
                        |. Parser.symbol_ "QUERY"
                        |. Parser.symbol_ "PLAN"
                    , Parser.succeed Explain
                    ]
            , Parser.succeed Nothing
            ]
        |= Parser.oneOf
            [ Parser.map CreateTable CreateTable.parser
            ]


toString : Statement -> String
toString statement =
    Types.ropeToString (toRope statement)


toRope : Statement -> Rope String
toRope statement =
    let
        prefix =
            case statement.explain of
                Nothing ->
                    Rope.empty

                Just Explain ->
                    Rope.singleton "EXPLAIN"

                Just ExplainQueryPlan ->
                    Rope.singleton "EXPLAIN QUERY PLAN"
    in
    prefix
        |> Rope.prependTo (toRopeInner statement.inner)


toRopeInner : InnerStatement -> Rope String
toRopeInner statement =
    case statement of
        AlterTable ever ->
            never ever

        Analyze ever ->
            never ever

        Attach ever ->
            never ever

        Begin ever ->
            never ever

        Commit ever ->
            never ever

        CreateIndex ever ->
            never ever

        CreateTable create ->
            CreateTable.toRope create

        CreateTrigger ever ->
            never ever

        CreateView ever ->
            never ever

        CreateVirtualTable ever ->
            never ever

        Delete ever ->
            never ever

        DeleteLimited ever ->
            never ever

        Detach ever ->
            never ever

        DropIndex ever ->
            never ever

        DropTable ever ->
            never ever

        DropTrigger ever ->
            never ever

        DropView ever ->
            never ever

        Insert ever ->
            never ever

        Pragma ever ->
            never ever

        Reindex ever ->
            never ever

        Release ever ->
            never ever

        Rollback ever ->
            never ever

        Savepoint ever ->
            never ever

        Select select ->
            Select.toRope select

        Update ever ->
            never ever

        UpdateLimited ever ->
            never ever

        Vacuum ever ->
            never ever
