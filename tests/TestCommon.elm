module TestCommon exposing (parseResultToString, testOutputRow, tokenizedToString, viewProblem)

import Ansi.Color
import Bitwise
import List.Extra
import Parser.OfTokens as Parser exposing (Node(..))
import Parser.Token as Token exposing (Token)
import Result.Extra
import Rope exposing (Rope)
import SQLite.Types as Types
import Triple.Extra


testOutputRow : String -> String -> String
testOutputRow label value =
    Ansi.Color.fontColor Ansi.Color.cyan label
        ++ ":\n"
        ++ indent 4 value


indent : Int -> String -> String
indent count input =
    let
        i : String
        i =
            String.repeat count " "
    in
    input
        |> String.split "\n"
        |> List.map (\line -> i ++ line)
        |> String.join "\n"


parseResultToString : (a -> Rope String) -> String -> Result (List (Parser.DeadEnd Token)) a -> String
parseResultToString toRope input result =
    case result of
        Err errs ->
            let
                lines : List String
                lines =
                    String.split "\n" input
            in
            errs
                |> List.Extra.gatherEqualsBy (\{ row, column } -> ( row, column ))
                |> List.map
                    (\( { row, column, problem }, others ) ->
                        let
                            ( problems, expecting ) =
                                (problem :: List.map .problem others)
                                    |> List.map errorToString
                                    |> Result.Extra.partition

                            expectingMessage : Maybe String
                            expectingMessage =
                                case expecting of
                                    [] ->
                                        Nothing

                                    [ e ] ->
                                        Just ("Expecting: " ++ e)

                                    es ->
                                        Just ("Expecting one of " ++ String.join " or " es)

                            problemsMessages : List String
                            problemsMessages =
                                List.map (\p -> "Problem: " ++ p) problems

                            allMessages : List String
                            allMessages =
                                case expectingMessage of
                                    Nothing ->
                                        problemsMessages

                                    Just msg ->
                                        msg :: problemsMessages
                        in
                        viewProblem lines row column allMessages
                    )
                |> String.join "\n\n--- OR ---\n\n"

        Ok s ->
            Types.ropeToString (toRope s) ++ "\n" ++ Ansi.Color.fontColor Ansi.Color.brightBlack (Debug.toString s)


errorToString : Parser.Error Token -> Result String String
errorToString error =
    case error of
        Parser.Problem p ->
            Ok p

        Parser.ExpectingEnd ->
            Err "end"

        Parser.ExpectingToken t ->
            Err (Token.toString t)


viewProblem : List String -> Int -> Int -> List String -> String
viewProblem lines row column messages =
    let
        pre : String
        pre =
            lines
                |> List.drop (row - 3)
                |> List.take (min 2 (row - 1))
                |> List.map (\line -> line ++ "\n")
                |> String.concat

        current : String
        current =
            lines
                |> List.drop (row - 1)
                |> List.take 1
                |> String.join "\n"

        post : String
        post =
            lines
                |> List.drop row
                |> List.take 2
                |> String.join "\n"

        addMarker : Int -> String -> String
        addMarker index problemLine =
            if index == 0 then
                String.repeat (column - 1) " " ++ Ansi.Color.fontColor Ansi.Color.brightRed "^-- " ++ problemLine

            else
                String.repeat (column + 3) " " ++ problemLine
    in
    pre
        ++ current
        ++ "\n"
        ++ (messages
                |> List.indexedMap addMarker
                |> String.join "\n"
           )
        ++ "\n"
        ++ post


tokenizedToString : List (Node Token) -> String -> String
tokenizedToString tokens input =
    let
        tokensWithColors =
            tokens |> List.indexedMap colorToken

        coloredTokens =
            tokensWithColors
                |> List.map (\( Node _ t, color ) -> color ("[" ++ Debug.toString t ++ "]"))
                |> String.join " "

        coloredInput =
            String.foldl
                (\char ( acc, ( row, column ), tokensQueue ) ->
                    let
                        s : String
                        s =
                            String.fromChar char
                    in
                    case s of
                        "\n" ->
                            ( s :: acc, ( row + 1, 1 ), tokensQueue )

                        " " ->
                            ( s :: acc, ( row, column + 1 ), tokensQueue )

                        _ ->
                            let
                                ( color, newTokensQueue ) =
                                    getCurrentToken ( row, column ) tokensQueue
                            in
                            ( Maybe.withDefault identity color s :: acc, ( row, column + 1 ), newTokensQueue )
                )
                ( [], ( 1, 1 ), tokensWithColors )
                input
                |> Triple.Extra.first
                |> List.reverse
                |> String.concat
    in
    coloredInput ++ "\n\n" ++ coloredTokens


getCurrentToken :
    ( Int, Int )
    -> List ( Node token, color )
    -> ( Maybe color, List ( Node token, color ) )
getCurrentToken ( row, column ) queue =
    case queue of
        [] ->
            ( Nothing, queue )

        ( Node headRange _, headColor ) :: tail ->
            if (row < headRange.start.row) || (row == headRange.start.row && column < headRange.start.column) then
                ( Nothing, queue )

            else if (row > headRange.end.row) || (row == headRange.end.row && column >= headRange.end.column) then
                getCurrentToken ( row, column ) tail

            else
                ( Just headColor, queue )


colorToken : Int -> Node Token -> ( Node Token, String -> String )
colorToken i ((Node _ token) as node) =
    let
        get : Int -> Int -> Int
        get o v =
            v
                |> Bitwise.shiftRightBy o
                |> Bitwise.and 0xFF

        rgb : Int -> String -> String
        rgb v =
            { red = get 16 v
            , green = get 8 v
            , blue = get 0 v
            }
                |> Ansi.Color.rgb
                |> Ansi.Color.fontColor
    in
    ( node
    , case token of
        Token.Ident _ ->
            rgb 0x009CDCFE

        Token.Number _ ->
            rgb 0x00B5CEA8

        Token.String _ ->
            rgb 0x00CE9178

        Token.ParensOpen ->
            rgb 0x00CE9178

        Token.ParensClose ->
            rgb 0x00CE9178

        _ ->
            if String.all (\c -> Char.isAlpha c || c == '_') (Token.toString token) then
                rgb 0x00C586C0

            else
                rgb 0x00C0C0C0
    )
