module ParserTest exposing (justParseStatement, justParseStatement_, testParse)

import Expect
import Parser.OfTokens as Parser exposing (Parser)
import Parser.Token exposing (Token)
import Parser.Tokenizer
import Rope exposing (Rope)
import SQLite.Statement as Statement
import String.Multiline
import Test exposing (Test, test)
import TestCommon exposing (testOutputRow, tokenizedToString)


justParseStatement : String -> Test
justParseStatement input =
    let
        clean : String
        clean =
            String.Multiline.here input

        firstLine : String
        firstLine =
            clean
                |> String.split "\n"
                |> List.take 1
                |> String.concat
    in
    testParse firstLine Statement.parser Statement.toRope clean Nothing


justParseStatement_ : String -> String -> Test
justParseStatement_ label input =
    let
        clean : String
        clean =
            String.Multiline.here input

        firstLine : String
        firstLine =
            clean
                |> String.split "\n"
                |> List.take 1
                |> String.concat
    in
    testParse label Statement.parser Statement.toRope clean Nothing


testParse :
    String
    -> Parser Token a
    -> (a -> Rope String)
    -> String
    -> Maybe a
    -> Test
testParse label parser toRope input value =
    test label <|
        \_ ->
            case Parser.Tokenizer.tokenizer input of
                Ok tokenized ->
                    let
                        parsed : Result (List (Parser.DeadEnd Token)) a
                        parsed =
                            tokenized
                                |> Parser.run (parser |> Parser.skip Parser.end)
                    in
                    case value of
                        Just v ->
                            if parsed == Ok v then
                                Expect.pass

                            else
                                [ testOutputRow label input
                                , testOutputRow "Tokenized" (tokenizedToString tokenized)
                                , testOutputRow "Parsed" (TestCommon.parseResultToString toRope input parsed)
                                ]
                                    |> String.join "\n"
                                    |> Expect.fail

                        Nothing ->
                            case parsed of
                                Ok _ ->
                                    Expect.pass

                                Err _ ->
                                    [ testOutputRow label input
                                    , testOutputRow "Tokenized" (tokenizedToString tokenized)
                                    , testOutputRow "Parsed" (TestCommon.parseResultToString toRope input parsed)
                                    ]
                                        |> String.join "\n"
                                        |> Expect.fail

                Err ( location, e ) ->
                    let
                        lines : List String
                        lines =
                            String.split "\n" input
                    in
                    [ testOutputRow label input
                    , testOutputRow "Tokenized" (TestCommon.viewProblem lines location.row location.column [ e ])
                    ]
                        |> String.join "\n"
                        |> Expect.fail
