module ProjectId exposing (ProjectId, decoder, encoder, generator, toString)

import IdGenerator
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import Random exposing (Generator)


type ProjectId
    = ProjectId String


prefix : String
prefix =
    "pid_"


generator : Generator ProjectId
generator =
    IdGenerator.mapWithPrefix prefix ProjectId


toString : ProjectId -> String
toString (ProjectId str) =
    str


encoder : ProjectId -> Value
encoder (ProjectId str) =
    JE.string str


decoder : Decoder ProjectId
decoder =
    JD.map ProjectId JD.string
