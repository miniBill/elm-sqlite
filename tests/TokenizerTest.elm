module TokenizerTest exposing (..)

import Expect
import Parser.Tokenizer as Tokenizer
import Test exposing (Test)
import TestCommon exposing (testOutputRow, viewProblem)


nonAscii : Test
nonAscii =
    Test.test "Tokenizing non-ASCII characters" <|
        \_ ->
            let
                input : String
                input =
                    "'ᾀ'"
            in
            case Tokenizer.tokenizer input of
                Ok _ ->
                    Expect.pass

                Err ( location, e ) ->
                    let
                        lines : List String
                        lines =
                            String.split "\n" input
                    in
                    [ testOutputRow "Tokenizing non-ASCII characters" input
                    , testOutputRow "Tokenized" (TestCommon.viewProblem lines location.row location.column [ e ])
                    ]
                        |> String.join "\n"
                        |> Expect.fail
