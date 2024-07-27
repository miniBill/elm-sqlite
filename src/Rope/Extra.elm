module Rope.Extra exposing (intersperse)

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
