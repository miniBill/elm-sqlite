module SQLite.Statement exposing (Statement(..), toString)

import Rope exposing (Rope)
import SQLite.Statement.CreateTable as CreateTable
import SQLite.Statement.Select as Select
import SQLite.Types as Types


type Statement
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


toString : Statement -> String
toString statement =
    Types.ropeToString (toRope statement)


toRope : Statement -> Rope String
toRope statement =
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
