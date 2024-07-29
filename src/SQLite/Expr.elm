module SQLite.Expr exposing (Expr(..), LiteralValue(..), literalValueToString, toRope)

import Json.Encode
import Rope exposing (Rope)


type Expr
    = LiteralValue LiteralValue
    | OTHERS Never


type LiteralValue
    = NumericLiteral Float
    | StringLiteral String
    | BlobLiteral Never
    | Null
    | True
    | False
    | CurrentTime
    | CurrentDate
    | CurrentTimestamp


toRope : Expr -> Rope String
toRope expr =
    case expr of
        LiteralValue literal ->
            Rope.singleton (literalValueToString literal)

        OTHERS ever ->
            never ever


literalValueToString : LiteralValue -> String
literalValueToString literal =
    case literal of
        NumericLiteral numeric ->
            String.fromFloat numeric

        StringLiteral s ->
            let
                -- This is WRONG and BAD
                _ =
                    Debug.todo
            in
            Json.Encode.encode 0 (Json.Encode.string s)

        BlobLiteral _ ->
            Debug.todo "literalValueToString - branch 'BlobLiteral _' not implemented"

        Null ->
            Debug.todo "literalValueToString - branch 'Null' not implemented"

        True ->
            Debug.todo "literalValueToString - branch 'True' not implemented"

        False ->
            Debug.todo "literalValueToString - branch 'False' not implemented"

        CurrentTime ->
            Debug.todo "literalValueToString - branch 'CurrentTime' not implemented"

        CurrentDate ->
            Debug.todo "literalValueToString - branch 'CurrentDate' not implemented"

        CurrentTimestamp ->
            Debug.todo "literalValueToString - branch 'CurrentTimestamp' not implemented"
