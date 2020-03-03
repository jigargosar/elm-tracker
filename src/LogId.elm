module LogId exposing (LogId, decoder, encoder, generator, toString)

import IdGenerator
import Json.Decode exposing (Decoder)
import Json.Encode as JE exposing (Value)
import Random exposing (Generator)


type LogId
    = LogId String


prefix : String
prefix =
    "log_id_"


generator : Generator LogId
generator =
    IdGenerator.mapWithPrefix prefix LogId


toString : LogId -> String
toString (LogId str) =
    str


encoder : LogId -> Value
encoder (LogId str) =
    JE.string str


decoder : Decoder LogId
decoder =
    IdGenerator.decodeWhenPrefixed prefix LogId
