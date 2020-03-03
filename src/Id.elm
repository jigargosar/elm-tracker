module Id exposing (map)

import Random exposing (Generator)


map : (String -> id) -> Generator id
map func =
    Random.int 0 Random.maxInt |> Random.map (String.fromInt >> func)
