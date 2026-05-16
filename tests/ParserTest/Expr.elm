module ParserTest.Expr exposing (nPlus1, plusMinus)

import ParserTest
import SQLite.Expr as Expr
import Test exposing (Test)


nPlus1 : Test
nPlus1 =
    ParserTest.testParse "n + 1"
        Expr.parser
        Expr.toRope
        "n + 1"
        (Expr.plus (Expr.columnName "n") (Expr.int 1)
            |> Just
        )


plusMinus : Test
plusMinus =
    ParserTest.testParse "n + 1 - 2 + 3 - 4"
        Expr.parser
        Expr.toRope
        "n + 1 - 2 + 3 - 4"
        (Expr.minus
            (Expr.plus
                (Expr.minus
                    (Expr.plus (Expr.columnName "n") (Expr.int 1))
                    (Expr.int 2)
                )
                (Expr.int 3)
            )
            (Expr.int 4)
            |> Just
        )
