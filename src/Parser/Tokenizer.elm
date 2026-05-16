module Parser.Tokenizer exposing (tokenizer)

import List.Extra
import Parser.OfTokens exposing (Location, Node(..))
import Parser.Token as Token exposing (Token)


tokenizer : String -> Result ( Location, String ) (List (Node Token))
tokenizer input =
    let
        list : List Char
        list =
            String.toList input
    in
    tokenizerHelper
        { row = 1, column = 1 }
        (List.map (\c -> ( c, Char.toUpper c )) list)
        []


tokenizerHelper : Location -> List ( Char, Char ) -> List (Node Token) -> Result ( Location, String ) (List (Node Token))
tokenizerHelper position input acc =
    let
        simple : Int -> Token -> List ( Char, Char ) -> Result ( Location, String ) (List (Node Token))
        simple len token tail =
            let
                next : Location
                next =
                    { position | column = position.column + len }

                node : Node Token
                node =
                    Node { start = position, end = next } token
            in
            tokenizerHelper next tail (node :: acc)
    in
    case input of
        [] ->
            Ok (List.reverse acc)

        -- Parens
        ( '(', _ ) :: tail ->
            simple 1 Token.ParensOpen tail

        ( ')', _ ) :: tail ->
            simple 1 Token.ParensClose tail

        -- Operators
        ( '~', _ ) :: tail ->
            simple 1 Token.Tilde tail

        ( '+', _ ) :: tail ->
            simple 1 Token.Plus tail

        ( '-', _ ) :: tail ->
            simple 1 Token.Minus tail

        ( '|', _ ) :: ( '|', _ ) :: tail ->
            simple 2 Token.OrSymbol tail

        ( '.', _ ) :: tail ->
            simple 1 Token.Dot tail

        ( '*', _ ) :: tail ->
            simple 1 Token.Star tail

        ( '<', _ ) :: ( '=', _ ) :: tail ->
            simple 2 Token.LessThanOrEquals tail

        ( '<', _ ) :: ( '>', _ ) :: tail ->
            simple 2 Token.Different tail

        ( '<', _ ) :: tail ->
            simple 1 Token.LessThan tail

        ( '>', _ ) :: ( '=', _ ) :: tail ->
            simple 2 Token.GreaterThanOrEquals tail

        ( '>', _ ) :: tail ->
            simple 1 Token.GreaterThan tail

        ( '=', _ ) :: tail ->
            simple 1 Token.Equals tail

        -- Other
        ( ',', _ ) :: tail ->
            simple 1 Token.Comma tail

        ( ';', _ ) :: tail ->
            simple 1 Token.Semicolon tail

        -- Whitespace
        ( ' ', _ ) :: tail ->
            tokenizerHelper { position | column = position.column + 1 } tail acc

        ( '\n', _ ) :: tail ->
            tokenizerHelper { column = 1, row = position.row + 1 } tail acc

        -- Strings
        ( '\'', _ ) :: tail ->
            let
                positionAfterQuote : Location
                positionAfterQuote =
                    { position | column = position.column + 1 }
            in
            case chompString positionAfterQuote tail [] of
                Err e ->
                    Err e

                Ok ( string, ( newPosition, newInput ) ) ->
                    let
                        token : Node Token
                        token =
                            Node { start = position, end = newPosition } (Token.String string)
                    in
                    tokenizerHelper newPosition newInput (token :: acc)

        -- Identifiers, numbers and keywords
        ( head, _ ) :: _ ->
            if Char.isAlpha head then
                let
                    ( ( tokenContent, tokenUppercaseContent ), newInput ) =
                        id input

                    next : Location
                    next =
                        { position | column = position.column + String.length tokenContent }

                    node : a -> Node a
                    node t =
                        Node { start = position, end = next } t

                    token : Token
                    token =
                        Token.fromString tokenUppercaseContent
                            |> Maybe.withDefault (Token.Ident tokenContent)
                in
                tokenizerHelper next newInput (node token :: acc)

            else if Char.isDigit head then
                let
                    ( tokenContent, newInput ) =
                        input
                            |> List.Extra.span (\( c, _ ) -> c == '.' || Char.isDigit c)
                            |> Tuple.mapFirst (\s -> s |> List.unzip |> Tuple.first |> String.fromList)
                in
                case String.toFloat tokenContent of
                    Nothing ->
                        Err ( position, "Could not parse " ++ tokenContent ++ " as number" )

                    Just f ->
                        let
                            next : Location
                            next =
                                { position | column = position.column + String.length tokenContent }

                            node : a -> Node a
                            node t =
                                Node { start = position, end = next } t
                        in
                        tokenizerHelper next newInput (node (Token.Number f) :: acc)

            else
                Err ( position, "Unexpected char '" ++ String.fromChar head ++ "'" )


chompString : Location -> List ( Char, Char ) -> List Char -> Result ( Location, String ) ( String, ( Location, List ( Char, Char ) ) )
chompString position input acc =
    case input of
        ( '\'', _ ) :: ( '\'', _ ) :: tail ->
            chompString
                { position | column = position.column + 2 }
                tail
                ('\'' :: acc)

        ( '\'', _ ) :: tail ->
            ( String.fromList (List.reverse acc)
            , ( { position | column = position.column + 1 }, tail )
            )
                |> Ok

        ( '\n', _ ) :: tail ->
            chompString
                { row = position.row + 1, column = 1 }
                tail
                ('\n' :: acc)

        ( c, _ ) :: tail ->
            chompString
                { position | column = position.column + 1 }
                tail
                (c :: acc)

        [] ->
            Err ( position, "Unexpected end of input while reading string" )


id : List ( Char, Char ) -> ( ( String, String ), List ( Char, Char ) )
id input =
    let
        ( before, after ) =
            List.Extra.span (\( c, _ ) -> isIdChar c) input

        ( ls, us ) =
            List.unzip before
    in
    ( ( String.fromList ls, String.fromList us ), after )


isIdChar : Char -> Bool
isIdChar c =
    c == '_' || Char.isAlpha c || Char.isDigit c
