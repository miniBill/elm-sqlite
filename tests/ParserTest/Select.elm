module ParserTest.Select exposing (selectStar)

import ParserTest
import Test exposing (Test)


selectStar : Test
selectStar =
    ParserTest.justParseStatement """SELECT * FROM t"""
