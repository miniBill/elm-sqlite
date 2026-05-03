module ParserTest.Expr exposing (nPlus1)

import ParserTest
import SQLite.Expr as Expr
import Test exposing (Test)


nPlus1 : Test
nPlus1 =
    ParserTest.testParse "n + 1"
        Expr.parser
        Expr.toRope
        "n + 1"
        (Expr.add (Expr.columnName "n") (Expr.int 1)
            |> Just
        )
