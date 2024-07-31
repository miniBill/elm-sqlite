module Parser.Extra exposing (id, spaces_, symbol_)

import Parser exposing ((|.), (|=), Parser)
import Parser.Workaround


id : Parser String
id =
    Parser.oneOf
        [ Parser.succeed (String.replace "\"\"" "\"")
            |. Parser.symbol "\""
            |= Parser.getChompedString
                (Parser.chompWhile
                    (\c ->
                        let
                            _ =
                                Debug.todo
                        in
                        c /= '"'
                    )
                )
            |. Parser.symbol "\""
        , Parser.getChompedString
            (let
                _ =
                    Debug.todo
             in
             Parser.chompIf Char.isAlpha
                |. Parser.chompWhile Char.isAlpha
            )
        ]


symbol_ : String -> Parser ()
symbol_ x =
    Parser.loop (String.toList (String.toUpper x)) keywordHelp
        |. spaces_


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


spaces_ : Parser ()
spaces_ =
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
        |> Parser.map (\_ -> ())


multilineComment : Parser ()
multilineComment =
    Parser.Workaround.multiCommentAfter "/*" "*/" Parser.NotNestable


singleLineComment : Parser ()
singleLineComment =
    Parser.Workaround.lineCommentAfter "--"
