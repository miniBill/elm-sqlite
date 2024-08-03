module SQLite.Statement.Select exposing (Statement(..), parser, toRope)

import Parser.OfTokens as Parser exposing (Parser)
import Rope exposing (Rope)


type Statement
    = Statement Never


toRope : Statement -> Rope String
toRope (Statement ever) =
    never ever


parser : Parser token Statement
parser =
    Parser.problem "Statement.Select.parser"
