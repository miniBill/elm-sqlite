module SQLite.Expr exposing (Expr(..), toRope)

import Rope exposing (Rope)


type Expr
    = Expr Never


toRope : Expr -> Rope String
toRope (Expr ever) =
    never ever
