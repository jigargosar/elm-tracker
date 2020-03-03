module ProjectId exposing (ProjectId, decoder, encoder, generator, toString)

import IdGenerator
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import Random exposing (Generator)


type ProjectId
    = ProjectId String


generator : Generator ProjectId
generator =
    IdGenerator.map ProjectId


toString : ProjectId -> String
toString (ProjectId str) =
    str


encoder : ProjectId -> Value
encoder (ProjectId str) =
    JE.string str


decoder : Decoder ProjectId
decoder =
    JD.map ProjectId JD.string
