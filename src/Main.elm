module Main exposing (main)

import Html exposing (text)
import Random exposing (Generator)


type alias Project =
    { id : ProjectId
    }


type ProjectId
    = ProjectId String


idGen : (String -> id) -> Generator id
idGen tag =
    Random.int 0 Random.maxInt |> Random.map (String.fromInt >> tag)


main =
    text "Foo"
