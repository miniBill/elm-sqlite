module ParserTest exposing (tableCreation)

import Expect
import Parser.OfTokens as Parser exposing (Parser)
import Parser.Token exposing (Token)
import Parser.Tokenizer
import Rope
import SQLite.Statement as Statement
import Test exposing (Test, test)
import TestCommon exposing (testOutputRow, tokenizedToString)


tableCreation : Test
tableCreation =
    testParse "Table creation"
        (Parser.many Statement.parser)
        (\statements ->
            statements
                |> List.map Statement.toRope
                |> Rope.fromList
                |> Rope.concat
        )
        """
        """
        []


testParse :
    String
    -> Parser Token a
    -> (a -> Rope.Rope String)
    -> String
    -> a
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
                    if parsed == Ok value then
                        Expect.pass

                    else
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
