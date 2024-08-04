module Parser.Token exposing (Token(..), fromString, toString)

{-|

@docs Token, fromString, toString

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
    | Ident String
    | Number Float
    | String String


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

        "INTEGER" ->
            Just Integer

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


toString : Token -> String
toString input =
    case input of
        Abort ->
            "ABORT"

        Action ->
            "ACTION"

        Add ->
            "ADD"

        After ->
            "AFTER"

        All ->
            "ALL"

        Alter ->
            "ALTER"

        Always ->
            "ALWAYS"

        Analyze ->
            "ANALYZE"

        And ->
            "AND"

        As ->
            "AS"

        Asc ->
            "ASC"

        Attach ->
            "ATTACH"

        AutoIncrement ->
            "AUTOINCREMENT"

        Before ->
            "BEFORE"

        Begin ->
            "BEGIN"

        Between ->
            "BETWEEN"

        By ->
            "BY"

        Cascade ->
            "CASCADE"

        Case ->
            "CASE"

        Cast ->
            "CAST"

        Check ->
            "CHECK"

        Collate ->
            "COLLATE"

        Column ->
            "COLUMN"

        Commit ->
            "COMMIT"

        Conflict ->
            "CONFLICT"

        Constraint ->
            "CONSTRAINT"

        Create ->
            "CREATE"

        Cross ->
            "CROSS"

        Current ->
            "CURRENT"

        Current_Date ->
            "CURRENT_DATE"

        Current_Time ->
            "CURRENT_TIME"

        Current_Timestamp ->
            "CURRENT_TIMESTAMP"

        Database ->
            "DATABASE"

        Default ->
            "DEFAULT"

        Deferrable ->
            "DEFERRABLE"

        Deferred ->
            "DEFERRED"

        Delete ->
            "DELETE"

        Desc ->
            "DESC"

        Detach ->
            "DETACH"

        Distinct ->
            "DISTINCT"

        Do ->
            "DO"

        Drop ->
            "DROP"

        Each ->
            "EACH"

        Else ->
            "ELSE"

        End ->
            "END"

        Escape ->
            "ESCAPE"

        Except ->
            "EXCEPT"

        Exclude ->
            "EXCLUDE"

        Exclusive ->
            "EXCLUSIVE"

        Exists ->
            "EXISTS"

        Explain ->
            "EXPLAIN"

        Fail ->
            "FAIL"

        Filter ->
            "FILTER"

        First ->
            "FIRST"

        Following ->
            "FOLLOWING"

        For ->
            "FOR"

        Foreign ->
            "FOREIGN"

        From ->
            "FROM"

        Full ->
            "FULL"

        Generated ->
            "GENERATED"

        Glob ->
            "GLOB"

        Group ->
            "GROUP"

        Groups ->
            "GROUPS"

        Having ->
            "HAVING"

        If ->
            "IF"

        Ignore ->
            "IGNORE"

        Immediate ->
            "IMMEDIATE"

        In ->
            "IN"

        Index ->
            "INDEX"

        Indexed ->
            "INDEXED"

        Initially ->
            "INITIALLY"

        Inner ->
            "INNER"

        Insert ->
            "INSERT"

        Instead ->
            "INSTEAD"

        Integer ->
            "INTEGER"

        Intersect ->
            "INTERSECT"

        Into ->
            "INTO"

        Is ->
            "IS"

        IsNull ->
            "ISNULL"

        Join ->
            "JOIN"

        Key ->
            "KEY"

        Last ->
            "LAST"

        Left ->
            "LEFT"

        Like ->
            "LIKE"

        Limit ->
            "LIMIT"

        Match ->
            "MATCH"

        Materialized ->
            "MATERIALIZED"

        Natural ->
            "NATURAL"

        No ->
            "NO"

        Not ->
            "NOT"

        Nothing ->
            "NOTHING"

        NotNull ->
            "NOTNULL"

        Null ->
            "NULL"

        Nulls ->
            "NULLS"

        Of ->
            "OF"

        Offset ->
            "OFFSET"

        On ->
            "ON"

        Or ->
            "OR"

        Order ->
            "ORDER"

        Others ->
            "OTHERS"

        Outer ->
            "OUTER"

        Over ->
            "OVER"

        Partition ->
            "PARTITION"

        Plan ->
            "PLAN"

        Pragma ->
            "PRAGMA"

        Preceding ->
            "PRECEDING"

        Primary ->
            "PRIMARY"

        Query ->
            "QUERY"

        Raise ->
            "RAISE"

        Range ->
            "RANGE"

        Recursive ->
            "RECURSIVE"

        References ->
            "REFERENCES"

        Regexp ->
            "REGEXP"

        Reindex ->
            "REINDEX"

        Release ->
            "RELEASE"

        Rename ->
            "RENAME"

        Replace ->
            "REPLACE"

        Restrict ->
            "RESTRICT"

        Returning ->
            "RETURNING"

        Right ->
            "RIGHT"

        Rollback ->
            "ROLLBACK"

        Row ->
            "ROW"

        Rows ->
            "ROWS"

        Savepoint ->
            "SAVEPOINT"

        Select ->
            "SELECT"

        Set ->
            "SET"

        Table ->
            "TABLE"

        Temp ->
            "TEMP"

        Temporary ->
            "TEMPORARY"

        Then ->
            "THEN"

        Ties ->
            "TIES"

        To ->
            "TO"

        Transaction ->
            "TRANSACTION"

        Trigger ->
            "TRIGGER"

        Unbounded ->
            "UNBOUNDED"

        Union ->
            "UNION"

        Unique ->
            "UNIQUE"

        Update ->
            "UPDATE"

        Using ->
            "USING"

        Vacuum ->
            "VACUUM"

        Values ->
            "VALUES"

        View ->
            "VIEW"

        Virtual ->
            "VIRTUAL"

        When ->
            "WHEN"

        Where ->
            "WHERE"

        Window ->
            "WINDOW"

        With ->
            "WITH"

        Without ->
            "WITHOUT"

        ParensClose ->
            ")"

        ParensOpen ->
            "("

        Comma ->
            ","

        Dot ->
            "."

        Ident i ->
            i

        Number f ->
            String.fromFloat f

        String s ->
            "'" ++ String.replace "'" "''" s ++ "'"
