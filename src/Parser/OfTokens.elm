module Parser.OfTokens exposing (Error(..), PStep(..), Parser, Trailing(..), custom_, end, keep, map, oneOf, oneOf_, problem, run, sequence_, skip, succeed, token, token_)

{-|

@docs Error, PStep, Parser, Trailing, custom_, end, keep, map, oneOf, oneOf_, problem, run, sequence_, skip, succeed, token, token_

-}

import Rope exposing (Rope)


type Parser token a
    = Parser (List token -> PStep token a)


{-| What’s the deal with trailing commas? Are they `Forbidden`?
Are they `Optional`? Are they `Mandatory`?
-}
type Trailing
    = Forbidden
    | Optional
    | Mandatory


type PStep token a
    = Bad Bool (Rope (Error token))
    | Good Bool a (List token)


type Error token
    = Problem String
    | ExpectingEnd
    | ExpectingToken token
    | ExpectingTableNameOrSchemaNameAndTableName


custom : (List token -> PStep token a) -> Parser token a
custom f =
    Parser f


custom_ :
    (List token -> PStep token a)
    -> Parser token (a -> b)
    -> Parser token b
custom_ f main =
    main |> keep (custom f)


keep : Parser token a -> Parser token (a -> b) -> Parser token b
keep (Parser second) (Parser first) =
    Parser
        (\stream ->
            case first stream of
                Bad firstCommitted e ->
                    Bad firstCommitted e

                Good firstCommitted f tail ->
                    case second tail of
                        Bad secondCommitted e ->
                            Bad secondCommitted e

                        Good secondCommitted s next ->
                            Good (firstCommitted || secondCommitted) (f s) next
        )


skip : Parser token () -> Parser token a -> Parser token a
skip (Parser second) (Parser first) =
    Parser
        (\stream ->
            case first stream of
                Bad firstCommitted e ->
                    Bad firstCommitted e

                Good firstCommitted f tail ->
                    case second tail of
                        Bad secondCommitted e ->
                            Bad secondCommitted e

                        Good secondCommitted () next ->
                            Good (firstCommitted || secondCommitted) f next
        )


oneOf_ : List (Parser token a) -> Parser token (a -> b) -> Parser token b
oneOf_ alternatives other =
    keep (oneOf alternatives) other


oneOf : List (Parser token a) -> Parser token a
oneOf list =
    Parser
        (\stream ->
            oneOfHelper Rope.empty list stream
        )


oneOfHelper : Rope (Error token) -> List (Parser token a) -> List token -> PStep token a
oneOfHelper errs list stream =
    case list of
        [] ->
            Bad False errs

        (Parser head) :: tail ->
            case head stream of
                Good committed v rest ->
                    Good committed v rest

                Bad True e ->
                    Bad True e

                Bad False e ->
                    oneOfHelper (errs |> Rope.prependTo e) tail stream


token_ : a -> Parser a b -> Parser a b
token_ t main =
    skip (token t) main


token : token -> Parser token ()
token t =
    Parser
        (\stream ->
            case stream of
                [] ->
                    Bad False (Rope.singleton (ExpectingToken t))

                head :: tail ->
                    if head == t then
                        Good True () tail

                    else
                        Bad False (Rope.singleton (ExpectingToken t))
        )


succeed : a -> Parser token a
succeed res =
    Parser (\stream -> Good False res stream)


map : (a -> b) -> Parser token a -> Parser token b
map f (Parser p) =
    Parser
        (\stream ->
            case p stream of
                Good commit v rest ->
                    Good commit (f v) rest

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
        (\stream ->
            let
                (Parser inner) =
                    config.item

                go : Bool -> List item -> List token -> PStep token (List item)
                go first acc queue =
                    case queue of
                        [] ->
                            ExpectingToken config.end
                                |> Rope.singleton
                                |> Bad True

                        head :: tail ->
                            if head == config.end && (first || config.trailing == Optional) then
                                Good True (List.reverse acc) tail

                            else if first then
                                case inner queue of
                                    Bad _ errs ->
                                        Bad True errs

                                    Good _ result rest ->
                                        go False (result :: acc) rest

                            else if head /= config.separator then
                                ExpectingToken config.separator
                                    |> Rope.singleton
                                    |> Bad True

                            else if config.trailing == Forbidden then
                                case inner tail of
                                    Bad _ errs ->
                                        Bad True errs

                                    Good _ result rest ->
                                        go False (result :: acc) rest

                            else if List.head tail == Just config.end then
                                Good True (List.reverse acc) (List.drop 1 tail)

                            else
                                case inner tail of
                                    Bad _ errs ->
                                        Bad True errs

                                    Good _ result rest ->
                                        go False (result :: acc) rest
            in
            case stream of
                [] ->
                    Bad False (Rope.singleton (ExpectingToken config.start))

                head :: tail ->
                    if head == config.start then
                        go True [] tail

                    else
                        Bad False (Rope.singleton (ExpectingToken config.start))
        )


problem : String -> Parser token v
problem msg =
    Parser (\_ -> Bad False (Rope.singleton (Problem msg)))


end : Parser token ()
end =
    Parser
        (\stream ->
            if List.isEmpty stream then
                Good True () []

            else
                Bad False (Rope.singleton ExpectingEnd)
        )


run : Parser token a -> List token -> Result (List (Error token)) a
run (Parser parser) stream =
    case parser stream of
        Good _ result _ ->
            Ok result

        Bad _ errors ->
            Err (Rope.toList errors)
