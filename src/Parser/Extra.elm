module Parser.Extra exposing (keyword_)

import Parser exposing ((|.), Parser)
import Parser.Workaround


keyword_ : String -> Parser ()
keyword_ x =
    Parser.loop (String.toList (String.toUpper x)) keywordHelp
        |. spaces


keywordHelp : List Char -> Parser (Parser.Step (List Char) ())
keywordHelp queue =
    case queue of
        [] ->
            Parser.commit (Parser.Done ())

        head :: tail ->
            Parser.map
                (\_ -> Parser.Loop tail)
                (Parser.chompIf (\c -> Char.toUpper c == head)
                    |> Parser.backtrackable
                )


spaces : Parser (List ())
spaces =
    Parser.sequence
        { start = ""
        , end = ""
        , spaces = Parser.chompWhile (\c -> c == ' ' || c == '\n' || c == '\u{000D}' || c == '\t')
        , item =
            Parser.oneOf
                [ multilineComment
                , singleLineComment
                ]
        , separator = ""
        , trailing = Parser.Optional
        }


multilineComment : Parser ()
multilineComment =
    Parser.Workaround.multiCommentAfter "/*" "*/" Parser.NotNestable


singleLineComment : Parser ()
singleLineComment =
    Parser.Workaround.lineCommentAfter "--"
