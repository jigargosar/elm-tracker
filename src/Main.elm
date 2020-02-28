module Main exposing (main)

import Html exposing (text)
import Random exposing (Generator)


type alias Project =
    { id : ProjectId
    , title : String
    }


type ProjectId
    = ProjectId String


idGen : (String -> id) -> Generator id
idGen tag =
    Random.int 0 Random.maxInt |> Random.map (String.fromInt >> tag)


projectGen : String -> Generator Project
projectGen title =
    idGen
        (ProjectId
            >> (\id ->
                    { id = id
                    , title = title
                    }
               )
        )


main =
    text "Foo"
