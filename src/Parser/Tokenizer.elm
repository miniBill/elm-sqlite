module Parser.Tokenizer exposing (tokenizer)

import List.Extra
import Parser.Token as Token exposing (Token)


tokenizer : String -> Result String (List Token)
tokenizer input =
    tokenizerHelper
        (String.toList input)
        (String.toList (String.toUpper input))
        []


tokenizerHelper : List Char -> List Char -> List Token -> Result String (List Token)
tokenizerHelper input inputUppercase acc =
    case inputUppercase of
        [] ->
            Ok (List.reverse acc)

        '(' :: tail ->
            tokenizerHelper (List.drop 1 input) tail (Token.ParensOpen :: acc)

        ')' :: tail ->
            tokenizerHelper (List.drop 1 input) tail (Token.ParensClose :: acc)

        ',' :: tail ->
            tokenizerHelper (List.drop 1 input) tail (Token.Comma :: acc)

        '.' :: tail ->
            tokenizerHelper (List.drop 1 input) tail (Token.Dot :: acc)

        ' ' :: tail ->
            tokenizerHelper (List.drop 1 input) tail acc

        '\n' :: tail ->
            tokenizerHelper (List.drop 1 input) tail acc

        head :: _ ->
            if Char.isAlpha head then
                let
                    ( ( token, newInput ), ( tokenUppercase, newInputUppercase ) ) =
                        id input inputUppercase
                in
                case Token.fromString tokenUppercase of
                    Nothing ->
                        tokenizerHelper newInput newInputUppercase (Token.Ident token :: acc)

                    Just t ->
                        tokenizerHelper newInput newInputUppercase (t :: acc)

            else
                Err ("Unexpected char: " ++ String.fromChar head)


id : List Char -> List Char -> ( ( String, List Char ), ( String, List Char ) )
id input inputUppercase =
    ( List.Extra.span Char.isAlpha input
        |> Tuple.mapFirst String.fromList
    , List.Extra.span Char.isAlpha inputUppercase
        |> Tuple.mapFirst String.fromList
    )
