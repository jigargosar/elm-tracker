module IdGenerator exposing (decodeWhenPrefixed, map, mapWithPrefix)

import Json.Decode as JD exposing (Decoder)
import Json.Decode.Extra
import Random exposing (Generator)


map : (String -> id) -> Generator id
map func =
    Random.int 0 Random.maxInt |> Random.map (String.fromInt >> func)


mapWithPrefix : String -> (String -> id) -> Generator id
mapWithPrefix prefix func =
    map ((++) prefix >> func)


decodeWhenPrefixed : String -> (String -> id) -> Decoder id
decodeWhenPrefixed prefix func =
    JD.map func JD.string
