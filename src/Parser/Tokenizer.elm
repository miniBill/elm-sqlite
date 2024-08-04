module Parser.Tokenizer exposing (tokenizer)

import List.Extra
import Parser.OfTokens exposing (Location, Node(..))
import Parser.Token as Token exposing (Token)


tokenizer : String -> Result ( Location, String ) (List (Node Token))
tokenizer input =
    tokenizerHelper
        { row = 1, column = 1 }
        (String.toList input)
        (String.toList (String.toUpper input))
        []


tokenizerHelper : Location -> List Char -> List Char -> List (Node Token) -> Result ( Location, String ) (List (Node Token))
tokenizerHelper position input inputUppercase acc =
    let
        build : Token -> ( Location, Node Token )
        build t =
            let
                next : Location
                next =
                    { position | column = position.column + 1 }
            in
            ( next, Node { start = position, end = next } t )
    in
    case inputUppercase of
        [] ->
            Ok (List.reverse acc)

        '(' :: tail ->
            let
                ( next, token ) =
                    build Token.ParensOpen
            in
            tokenizerHelper next (List.drop 1 input) tail (token :: acc)

        ')' :: tail ->
            let
                ( next, token ) =
                    build Token.ParensClose
            in
            tokenizerHelper next (List.drop 1 input) tail (token :: acc)

        ',' :: tail ->
            let
                ( next, token ) =
                    build Token.Comma
            in
            tokenizerHelper next (List.drop 1 input) tail (token :: acc)

        '.' :: tail ->
            let
                ( next, token ) =
                    build Token.Dot
            in
            tokenizerHelper next (List.drop 1 input) tail (token :: acc)

        ' ' :: tail ->
            tokenizerHelper { position | column = position.column + 1 } (List.drop 1 input) tail acc

        '\n' :: tail ->
            tokenizerHelper { column = 1, row = position.row + 1 } (List.drop 1 input) tail acc

        '\'' :: tail ->
            case chompString position (List.drop 1 input) tail [] of
                Err e ->
                    Err e

                Ok ( string, ( newPosition, newInput, newInputUppercase ) ) ->
                    let
                        token : Node Token
                        token =
                            Node { start = position, end = newPosition } (Token.String string)
                    in
                    tokenizerHelper newPosition newInput newInputUppercase (token :: acc)

        head :: _ ->
            if Char.isAlpha head then
                let
                    ( ( tokenContent, newInput ), ( tokenUppercaseContent, newInputUppercase ) ) =
                        id input inputUppercase

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
                tokenizerHelper next newInput newInputUppercase (node token :: acc)

            else if Char.isDigit head then
                let
                    ( tokenContent, newInput ) =
                        input
                            |> List.Extra.span (\c -> c == '.' || Char.isDigit c)
                            |> Tuple.mapFirst String.fromList
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

                            newInputUppercase : List Char
                            newInputUppercase =
                                inputUppercase
                                    |> List.Extra.dropWhile (\c -> c == '.' || Char.isDigit c)
                        in
                        tokenizerHelper next newInput newInputUppercase (node (Token.Number f) :: acc)

            else
                Err ( position, "Unexpected char: " ++ String.fromChar head )


chompString : Location -> List Char -> List Char -> List Char -> Result ( Location, String ) ( String, ( Location, List Char, List Char ) )
chompString position input inputUppercase acc =
    case input of
        '\'' :: '\'' :: tail ->
            chompString
                { position | column = position.column + 2 }
                tail
                (List.drop 2 inputUppercase)
                ('\'' :: acc)

        '\'' :: tail ->
            ( String.fromList (List.reverse acc)
            , ( { position | column = position.column + 1 }, tail, List.drop 1 inputUppercase )
            )
                |> Ok

        '\n' :: tail ->
            chompString
                { row = position.row + 1, column = 1 }
                tail
                (List.drop 1 inputUppercase)
                ('\n' :: acc)

        c :: tail ->
            chompString
                { position | column = position.column + 1 }
                tail
                (List.drop 1 inputUppercase)
                (c :: acc)

        [] ->
            Err ( position, "Unexpected end of input while reading string" )


id : List Char -> List Char -> ( ( String, List Char ), ( String, List Char ) )
id input inputUppercase =
    ( List.Extra.span Char.isAlpha input
        |> Tuple.mapFirst String.fromList
    , List.Extra.span Char.isAlpha inputUppercase
        |> Tuple.mapFirst String.fromList
    )
