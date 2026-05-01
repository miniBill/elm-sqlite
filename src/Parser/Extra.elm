module Parser.Extra exposing (ident, ident_)

import Parser.OfTokens as Parser exposing (Error, Node(..), PStep(..), Parser)
import Parser.Token as Token exposing (Token)
import Rope


ident_ : Parser Token (String -> a) -> Parser Token a
ident_ other =
    Parser.keep ident other


ident : Parser Token String
ident =
    Parser.custom
        (\position stream ->
            case stream of
                (Node pos (Token.Ident i)) :: tail ->
                    Good True i pos.end tail

                _ ->
                    Parser.errorAt False position (Parser.ExpectingToken (Token.Ident "<an identifier>"))
        )
