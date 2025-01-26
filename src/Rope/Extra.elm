module Rope.Extra exposing (appendIf, appendMaybe, fromListOfRopes, intersperse)

import Rope exposing (Rope)


intersperse : a -> Rope a -> Rope a
intersperse separator rope =
    Rope.foldl
        (\e acc ->
            if Rope.isEmpty acc then
                Rope.singleton e

            else
                acc |> Rope.append separator |> Rope.append e
        )
        Rope.empty
        rope


appendIf : Bool -> a -> Rope a -> Rope a
appendIf cond el rope =
    if cond then
        rope |> Rope.append el

    else
        rope


appendMaybe : (a -> Rope b) -> Maybe a -> Rope b -> Rope b
appendMaybe f value rope =
    case value of
        Nothing ->
            rope

        Just x ->
            rope |> Rope.prependTo (f x)


fromListOfRopes : List (Rope String) -> Rope String
fromListOfRopes list =
    list
        |> Rope.fromList
        |> Rope.concat
