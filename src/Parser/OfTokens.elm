module Parser.OfTokens exposing
    ( DeadEnd, Error(..), Location, Node(..), PStep(..), Parser, Trailing(..), custom, custom_, end, errorAt, keep, map, oneOf, oneOf_, problem, run, sequence_, skip, succeed, token, token_
    , many, many_, maybe_
    )

{-|

@docs DeadEnd, Error, Location, Node, PStep, Parser, Trailing, custom, custom_, end, errorAt, keep, map, oneOf, oneOf_, problem, run, sequence_, skip, succeed, token, token_

-}

import Rope exposing (Rope)


type Node a
    = Node Range a


type alias Range =
    { start : Location
    , end : Location
    }


type alias Location =
    { row : Int
    , column : Int
    }


type alias DeadEnd token =
    { row : Int
    , column : Int
    , problem : Error token
    }


type Parser token a
    = Parser (Location -> List (Node token) -> PStep token a)


{-| What’s the deal with trailing commas? Are they `Forbidden`?
Are they `Optional`? Are they `Mandatory`?
-}
type Trailing
    = Forbidden
    | Optional
    | Mandatory


type PStep token a
    = Bad Bool (Rope (DeadEnd token))
    | Good Bool a Location (List (Node token))


type Error token
    = Problem String
    | ExpectingEnd
    | ExpectingToken token
    | ExpectingTableNameOrSchemaNameAndTableName


custom : (Location -> List (Node token) -> PStep token a) -> Parser token a
custom f =
    Parser f


custom_ :
    (Location -> List (Node token) -> PStep token a)
    -> Parser token (a -> b)
    -> Parser token b
custom_ f main =
    main |> keep (custom f)


keep : Parser token a -> Parser token (a -> b) -> Parser token b
keep (Parser second) (Parser first) =
    Parser
        (\position stream ->
            case first position stream of
                Bad firstCommitted e ->
                    Bad firstCommitted e

                Good firstCommitted f newPosition tail ->
                    case second newPosition tail of
                        Bad secondCommitted e ->
                            Bad (firstCommitted || secondCommitted) e

                        Good secondCommitted s lastPosition next ->
                            Good (firstCommitted || secondCommitted) (f s) lastPosition next
        )


skip : Parser token () -> Parser token a -> Parser token a
skip (Parser second) (Parser first) =
    Parser
        (\position stream ->
            case first position stream of
                Bad firstCommitted e ->
                    Bad firstCommitted e

                Good firstCommitted f newPosition tail ->
                    case second newPosition tail of
                        Bad secondCommitted e ->
                            Bad (firstCommitted || secondCommitted) e

                        Good secondCommitted () lastPosition next ->
                            Good (firstCommitted || secondCommitted) f lastPosition next
        )


oneOf_ : List (Parser token a) -> Parser token (a -> b) -> Parser token b
oneOf_ alternatives other =
    keep (oneOf alternatives) other


oneOf : List (Parser token a) -> Parser token a
oneOf list =
    Parser
        (\position stream ->
            oneOfHelper Rope.empty list position stream
        )


oneOfHelper : Rope (DeadEnd token) -> List (Parser token a) -> Location -> List (Node token) -> PStep token a
oneOfHelper errs list position stream =
    case list of
        [] ->
            Bad False errs

        (Parser head) :: tail ->
            case head position stream of
                Good committed v newPosition rest ->
                    Good committed v newPosition rest

                Bad True e ->
                    Bad True e

                Bad False e ->
                    oneOfHelper (errs |> Rope.prependTo e) tail position stream


token_ : a -> Parser a b -> Parser a b
token_ t main =
    skip (token t) main


token : token -> Parser token ()
token t =
    Parser
        (\position stream ->
            case stream of
                [] ->
                    errorAt False position (ExpectingToken t)

                (Node range head) :: tail ->
                    if head == t then
                        Good True () range.end tail

                    else
                        errorAt False position (ExpectingToken t)
        )


errorAt : Bool -> Location -> Error token -> PStep token a
errorAt commit location error =
    Rope.singleton
        { row = location.row
        , column = location.column
        , problem = error
        }
        |> Bad commit


succeed : a -> Parser token a
succeed res =
    Parser (\stream -> Good False res stream)


map : (a -> b) -> Parser token a -> Parser token b
map f (Parser p) =
    Parser
        (\position stream ->
            case p position stream of
                Good commit v newPosition rest ->
                    Good commit (f v) newPosition rest

                Bad commit e ->
                    Bad commit e
        )


sequence_ :
    { start : token
    , end : token
    , separator : token
    , item : Parser token item
    , trailing : Trailing
    }
    -> Parser token (List item -> a)
    -> Parser token a
sequence_ config main =
    main |> keep (sequence config)


sequence :
    { start : token
    , end : token
    , separator : token
    , item : Parser token item
    , trailing : Trailing
    }
    -> Parser token (List item)
sequence config =
    Parser
        (\initialPosition stream ->
            let
                (Parser inner) =
                    config.item

                go : Bool -> List item -> Location -> List (Node token) -> PStep token (List item)
                go first acc position queue =
                    case queue of
                        [] ->
                            errorAt True position (Problem "222" {- ExpectingToken config.end -})

                        (Node range head) :: tail ->
                            if head == config.end && (first || config.trailing /= Mandatory) then
                                Good True (List.reverse acc) range.end tail

                            else if first then
                                case inner position queue of
                                    Bad _ errs ->
                                        Bad True errs

                                    Good _ result newPosition rest ->
                                        go False (result :: acc) newPosition rest

                            else if head /= config.separator then
                                errorAt True range.start (Problem "237" {- ExpectingToken config.separator -})

                            else if config.trailing == Forbidden then
                                case inner position tail of
                                    Bad _ errs ->
                                        Bad True errs

                                    Good _ result newPosition rest ->
                                        go False (result :: acc) newPosition rest

                            else
                                case tail of
                                    (Node nextRange next) :: rest ->
                                        if next == config.end then
                                            Good True (List.reverse acc) nextRange.end rest

                                        else
                                            case inner position tail of
                                                Bad _ errs ->
                                                    Bad True errs

                                                Good _ result newPosition rest_ ->
                                                    go False (result :: acc) newPosition rest_

                                    [] ->
                                        case inner position tail of
                                            Bad _ errs ->
                                                Bad True errs

                                            Good _ result newPosition rest_ ->
                                                go False (result :: acc) newPosition rest_
            in
            case stream of
                [] ->
                    errorAt False initialPosition (ExpectingToken config.start)

                (Node range head) :: tail ->
                    if head == config.start then
                        go True [] range.end tail

                    else
                        errorAt False initialPosition (ExpectingToken config.start)
        )


problem : String -> Parser token v
problem msg =
    Parser
        (\position _ ->
            Bad False
                (Rope.singleton
                    { row = position.row
                    , column = position.column
                    , problem = Problem msg
                    }
                )
        )


end : Parser token ()
end =
    Parser
        (\position stream ->
            if List.isEmpty stream then
                Good True () position []

            else
                Bad False
                    (Rope.singleton
                        { row = position.row
                        , column = position.column
                        , problem = ExpectingEnd
                        }
                    )
        )


run : Parser token a -> List (Node token) -> Result (List (DeadEnd token)) a
run (Parser parser) stream =
    case parser { row = 1, column = 1 } stream of
        Good _ result _ _ ->
            Ok result

        Bad _ errors ->
            Err (Rope.toList errors)


maybe_ :
    Parser token a
    -> Parser token (Maybe a -> b)
    -> Parser token b
maybe_ inner main =
    main |> keep (maybe inner)


maybe : Parser token a -> Parser token (Maybe a)
maybe parser =
    oneOf
        [ map Just parser
        , succeed Nothing
        ]


many_ : Parser token a -> Parser token (List a -> b) -> Parser token b
many_ inner main =
    main |> keep (many inner)


many : Parser token a -> Parser token (List a)
many parser =
    Parser (\position stream -> manyHelper parser [] position stream)


manyHelper :
    Parser token a
    -> List a
    -> Location
    -> List (Node token)
    -> PStep token (List a)
manyHelper (Parser inner) acc position stream =
    case inner position stream of
        Bad True errs ->
            Bad True errs

        Bad False _ ->
            Good (not (List.isEmpty acc)) (List.reverse acc) position stream

        Good _ el newPosition newStream ->
            manyHelper (Parser inner) (el :: acc) newPosition newStream
