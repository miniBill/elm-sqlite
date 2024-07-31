module SQLite.Statement.Select exposing (Statement(..), parser, toRope)

import Parser exposing (Parser)
import Rope exposing (Rope)


type Statement
    = Statement Never


toRope : Statement -> Rope String
toRope (Statement ever) =
    never ever


parser : Parser Statement
parser =
    Parser.problem "Statement.Select.parser"
