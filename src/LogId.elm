module LogId exposing (LogId, decoder, encoder, foo, generator, toString)

import IdGenerator
import Json.Decode as JD exposing (Decoder)
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


foo : Result String LogId
foo =
    Random.step generator (Random.initialSeed 0)
        |> Tuple.first
        |> encoder
        |> JD.decodeValue decoder
        |> Result.mapError JD.errorToString
        |> Debug.log "debug"
