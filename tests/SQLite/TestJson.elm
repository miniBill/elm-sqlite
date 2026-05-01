module SQLite.TestJson exposing (..)

{-| -}

import List.NonEmpty as NonEmpty
import ParserTest
import SQLite.Expr as Expr
import SQLite.Statement as Statement
import SQLite.Statement.Select as Select
import Test exposing (Test)


{-| From SQLite's json tests README
-}
withRecursive : Test
withRecursive =
    ParserTest.testParse "TestJson.withRecursive"
        Select.parser
        Select.toRope
        """
            WITH RECURSIVE c(n) AS (VALUES(1) UNION ALL SELECT n+1 FROM c WHERE n<25)
            SELECT sum(json_valid(x)) FROM c, data1;
        """
        (Select.withRecursive
            (NonEmpty.singleton
                (Select.commonTableExpression "c"
                    [ "n" ]
                    (Select.unionAll
                        (Select.values (NonEmpty.singleton (NonEmpty.singleton (Expr.int 1))))
                        (Select.select_
                            (NonEmpty.singleton
                                (Select.expr
                                    (Expr.add (Expr.columnName "n") (Expr.int 1))
                                )
                            )
                            |> Select.from (NonEmpty.singleton "c")
                            |> Select.where_ (Expr.lt (Expr.columnName "n") (Expr.int 25))
                        )
                    )
                )
            )
            (Select.select_
                (NonEmpty.singleton (Select.expr (Expr.call "sum" [ Expr.call "json_valid" [ Expr.columnName "x" ] ])))
                |> Select.from ( "c", [ "data1" ] )
            )
            |> Just
        )
