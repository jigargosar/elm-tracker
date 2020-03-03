module IdGenerator exposing (map, mapWithPrefix)

import Random exposing (Generator)


map : (String -> id) -> Generator id
map func =
    Random.int 0 Random.maxInt |> Random.map (String.fromInt >> func)


mapWithPrefix : String -> (String -> id) -> Generator id
mapWithPrefix prefix func =
    map ((++) prefix >> func)
