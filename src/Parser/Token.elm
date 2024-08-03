module Parser.Token exposing
    ( Token(..)
    , fromString
    )

{-|

@docs Token

-}


type Token
    = Abort
    | Action
    | Add
    | After
    | All
    | Alter
    | Always
    | Analyze
    | And
    | As
    | Asc
    | Attach
    | AutoIncrement
    | Before
    | Begin
    | Between
    | By
    | Cascade
    | Case
    | Cast
    | Check
    | Collate
    | Column
    | Comma
    | Commit
    | Conflict
    | Constraint
    | Create
    | Cross
    | Current
    | Current_Date
    | Current_Time
    | Current_Timestamp
    | Database
    | Default
    | Deferrable
    | Deferred
    | Delete
    | Desc
    | Detach
    | Distinct
    | Do
    | Dot
    | Drop
    | Each
    | Else
    | End
    | Escape
    | Except
    | Exclude
    | Exclusive
    | Exists
    | Explain
    | Fail
    | Filter
    | First
    | Following
    | For
    | Foreign
    | From
    | Full
    | Generated
    | Glob
    | Group
    | Groups
    | Having
    | Ident String
    | If
    | Ignore
    | Immediate
    | In
    | Index
    | Indexed
    | Initially
    | Inner
    | Insert
    | Instead
    | Integer
    | Intersect
    | Into
    | Is
    | IsNull
    | Join
    | Key
    | Last
    | Left
    | Like
    | Limit
    | Match
    | Materialized
    | Natural
    | No
    | Not
    | Nothing
    | NotNull
    | Null
    | Nulls
    | Of
    | Offset
    | On
    | Or
    | Order
    | Others
    | Outer
    | Over
    | ParensClose
    | ParensOpen
    | Partition
    | Plan
    | Pragma
    | Preceding
    | Primary
    | Query
    | Raise
    | Range
    | Recursive
    | References
    | Regexp
    | Reindex
    | Release
    | Rename
    | Replace
    | Restrict
    | Returning
    | Right
    | Rollback
    | Row
    | Rows
    | Savepoint
    | Select
    | Set
    | Table
    | Temp
    | Temporary
    | Then
    | Ties
    | To
    | Transaction
    | Trigger
    | Unbounded
    | Union
    | Unique
    | Update
    | Using
    | Vacuum
    | Values
    | View
    | Virtual
    | When
    | Where
    | Window
    | With
    | Without


fromString : String -> Maybe Token
fromString input =
    case input of
        "ABORT" ->
            Just Abort

        "ACTION" ->
            Just Action

        "ADD" ->
            Just Add

        "AFTER" ->
            Just After

        "ALL" ->
            Just All

        "ALTER" ->
            Just Alter

        "ALWAYS" ->
            Just Always

        "ANALYZE" ->
            Just Analyze

        "AND" ->
            Just And

        "AS" ->
            Just As

        "ASC" ->
            Just Asc

        "ATTACH" ->
            Just Attach

        "AUTOINCREMENT" ->
            Just AutoIncrement

        "BEFORE" ->
            Just Before

        "BEGIN" ->
            Just Begin

        "BETWEEN" ->
            Just Between

        "BY" ->
            Just By

        "CASCADE" ->
            Just Cascade

        "CASE" ->
            Just Case

        "CAST" ->
            Just Cast

        "CHECK" ->
            Just Check

        "COLLATE" ->
            Just Collate

        "COLUMN" ->
            Just Column

        "COMMIT" ->
            Just Commit

        "CONFLICT" ->
            Just Conflict

        "CONSTRAINT" ->
            Just Constraint

        "CREATE" ->
            Just Create

        "CROSS" ->
            Just Cross

        "CURRENT" ->
            Just Current

        "CURRENT_DATE" ->
            Just Current_Date

        "CURRENT_TIME" ->
            Just Current_Time

        "CURRENT_TIMESTAMP" ->
            Just Current_Timestamp

        "DATABASE" ->
            Just Database

        "DEFAULT" ->
            Just Default

        "DEFERRABLE" ->
            Just Deferrable

        "DEFERRED" ->
            Just Deferred

        "DELETE" ->
            Just Delete

        "DESC" ->
            Just Desc

        "DETACH" ->
            Just Detach

        "DISTINCT" ->
            Just Distinct

        "DO" ->
            Just Do

        "DROP" ->
            Just Drop

        "EACH" ->
            Just Each

        "ELSE" ->
            Just Else

        "END" ->
            Just End

        "ESCAPE" ->
            Just Escape

        "EXCEPT" ->
            Just Except

        "EXCLUDE" ->
            Just Exclude

        "EXCLUSIVE" ->
            Just Exclusive

        "EXISTS" ->
            Just Exists

        "EXPLAIN" ->
            Just Explain

        "FAIL" ->
            Just Fail

        "FILTER" ->
            Just Filter

        "FIRST" ->
            Just First

        "FOLLOWING" ->
            Just Following

        "FOR" ->
            Just For

        "FOREIGN" ->
            Just Foreign

        "FROM" ->
            Just From

        "FULL" ->
            Just Full

        "GENERATED" ->
            Just Generated

        "GLOB" ->
            Just Glob

        "GROUP" ->
            Just Group

        "GROUPS" ->
            Just Groups

        "HAVING" ->
            Just Having

        "IF" ->
            Just If

        "IGNORE" ->
            Just Ignore

        "IMMEDIATE" ->
            Just Immediate

        "IN" ->
            Just In

        "INDEX" ->
            Just Index

        "INDEXED" ->
            Just Indexed

        "INITIALLY" ->
            Just Initially

        "INNER" ->
            Just Inner

        "INSERT" ->
            Just Insert

        "INSTEAD" ->
            Just Instead

        "INTERSECT" ->
            Just Intersect

        "INTO" ->
            Just Into

        "IS" ->
            Just Is

        "ISNULL" ->
            Just IsNull

        "JOIN" ->
            Just Join

        "KEY" ->
            Just Key

        "LAST" ->
            Just Last

        "LEFT" ->
            Just Left

        "LIKE" ->
            Just Like

        "LIMIT" ->
            Just Limit

        "MATCH" ->
            Just Match

        "MATERIALIZED" ->
            Just Materialized

        "NATURAL" ->
            Just Natural

        "NO" ->
            Just No

        "NOT" ->
            Just Not

        "NOTHING" ->
            Just Nothing

        "NOTNULL" ->
            Just NotNull

        "NULL" ->
            Just Null

        "NULLS" ->
            Just Nulls

        "OF" ->
            Just Of

        "OFFSET" ->
            Just Offset

        "ON" ->
            Just On

        "OR" ->
            Just Or

        "ORDER" ->
            Just Order

        "OTHERS" ->
            Just Others

        "OUTER" ->
            Just Outer

        "OVER" ->
            Just Over

        "PARTITION" ->
            Just Partition

        "PLAN" ->
            Just Plan

        "PRAGMA" ->
            Just Pragma

        "PRECEDING" ->
            Just Preceding

        "PRIMARY" ->
            Just Primary

        "QUERY" ->
            Just Query

        "RAISE" ->
            Just Raise

        "RANGE" ->
            Just Range

        "RECURSIVE" ->
            Just Recursive

        "REFERENCES" ->
            Just References

        "REGEXP" ->
            Just Regexp

        "REINDEX" ->
            Just Reindex

        "RELEASE" ->
            Just Release

        "RENAME" ->
            Just Rename

        "REPLACE" ->
            Just Replace

        "RESTRICT" ->
            Just Restrict

        "RETURNING" ->
            Just Returning

        "RIGHT" ->
            Just Right

        "ROLLBACK" ->
            Just Rollback

        "ROW" ->
            Just Row

        "ROWS" ->
            Just Rows

        "SAVEPOINT" ->
            Just Savepoint

        "SELECT" ->
            Just Select

        "SET" ->
            Just Set

        "TABLE" ->
            Just Table

        "TEMP" ->
            Just Temp

        "TEMPORARY" ->
            Just Temporary

        "THEN" ->
            Just Then

        "TIES" ->
            Just Ties

        "TO" ->
            Just To

        "TRANSACTION" ->
            Just Transaction

        "TRIGGER" ->
            Just Trigger

        "UNBOUNDED" ->
            Just Unbounded

        "UNION" ->
            Just Union

        "UNIQUE" ->
            Just Unique

        "UPDATE" ->
            Just Update

        "USING" ->
            Just Using

        "VACUUM" ->
            Just Vacuum

        "VALUES" ->
            Just Values

        "VIEW" ->
            Just View

        "VIRTUAL" ->
            Just Virtual

        "WHEN" ->
            Just When

        "WHERE" ->
            Just Where

        "WINDOW" ->
            Just Window

        "WITH" ->
            Just With

        "WITHOUT" ->
            Just Without

        _ ->
            Maybe.Nothing
