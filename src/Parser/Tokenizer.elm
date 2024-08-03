module Parser.Tokenizer exposing (tokenizer)

import List.Extra
import Parser.OfTokens exposing (Location, Node(..))
import Parser.Token as Token exposing (Token)


tokenizer : String -> Result String (List (Node Token))
tokenizer input =
    tokenizerHelper
        { row = 1, column = 1 }
        (String.toList input)
        (String.toList (String.toUpper input))
        []


tokenizerHelper : Location -> List Char -> List Char -> List (Node Token) -> Result String (List (Node Token))
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

            else
                Err ("Unexpected char: " ++ String.fromChar head)


id : List Char -> List Char -> ( ( String, List Char ), ( String, List Char ) )
id input inputUppercase =
    ( List.Extra.span Char.isAlpha input
        |> Tuple.mapFirst String.fromList
    , List.Extra.span Char.isAlpha inputUppercase
        |> Tuple.mapFirst String.fromList
    )
