module RoundtripTest exposing (suite)

import Ansi.Color
import Expect
import Fuzz exposing (Fuzzer)
import Fuzzers
import Parser.OfTokens as Parser exposing (Parser)
import Parser.Token exposing (Token)
import Parser.Tokenizer
import Rope exposing (Rope)
import SQLite.Expr as Expr
import SQLite.Statement as Statement
import SQLite.Statement.CreateTable as CreateTable
import SQLite.Types as Types
import Test exposing (Test, describe, fuzz)
import TestCommon exposing (parseResultToString, testOutputRow, tokenizedToString, viewProblem)


suite : Test
suite =
    describe "toString >> parse === Ok"
        [ checkRoundtrip "Statement" Fuzzers.statement Statement.toRope Statement.parser
        , checkRoundtrip "Column definition" Fuzzers.columnDefinition CreateTable.columnDefinitionToRope CreateTable.columnDefinitionParser
        , checkRoundtrip "Literal value" Fuzzers.literalValue Expr.literalValueToRope Expr.literalValueParser
        ]


checkRoundtrip : String -> Fuzzer a -> (a -> Rope String) -> Parser Token a -> Test
checkRoundtrip label fuzzer toRope parser =
    fuzz fuzzer label <|
        \value ->
            let
                valueString : String
                valueString =
                    value
                        |> toRope
                        |> Types.ropeToString

                valueView : String
                valueView =
                    valueString ++ "\n" ++ Ansi.Color.fontColor Ansi.Color.brightBlack (Debug.toString value)
            in
            case Parser.Tokenizer.tokenizer valueString of
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
                        [ testOutputRow label valueView
                        , testOutputRow "Tokenized" (tokenizedToString tokenized)
                        , testOutputRow "Parsed" (parseResultToString toRope valueString parsed)
                        ]
                            |> String.join "\n"
                            |> Expect.fail

                Err ( location, e ) ->
                    let
                        lines : List String
                        lines =
                            String.split "\n" valueString
                    in
                    [ testOutputRow label valueView
                    , testOutputRow "Tokenized" (viewProblem lines location.row location.column [ e ])
                    ]
                        |> String.join "\n"
                        |> Expect.fail
