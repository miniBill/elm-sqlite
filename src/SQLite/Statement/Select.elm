module SQLite.Statement.Select exposing (Statement(..), toRope)

import Rope exposing (Rope)


type Statement
    = Statement Never


toRope : Statement -> Rope String
toRope (Statement ever) =
    never ever
