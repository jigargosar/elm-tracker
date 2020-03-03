module ProjectId exposing (ProjectId, decoder, encoder, generator, toString)

import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import Random exposing (Generator)


type ProjectId
    = ProjectId String


idGen : (String -> id) -> Generator id
idGen tag =
    Random.int 0 Random.maxInt |> Random.map (String.fromInt >> tag)


generator : Generator ProjectId
generator =
    idGen ProjectId


toString : ProjectId -> String
toString (ProjectId str) =
    str


encoder : ProjectId -> Value
encoder (ProjectId str) =
    JE.string str


decoder : Decoder ProjectId
decoder =
    JD.map ProjectId JD.string
