module Log exposing
    ( Log, generator
    , idString, projectId
    , startDate, endMillis
    , sumTracked
    , decoder, encoder, startMillis
    )

{-|

@docs Log, generator

@docs idString, projectId

@docs startDate, endMillis

@docs sumTracked

-}

import Date exposing (Date)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import LogId exposing (LogId)
import ProjectId exposing (ProjectId)
import Random exposing (Generator)
import Time exposing (Posix, Zone)
import TypedTime exposing (TypedTime)



-- PUBLIC


type alias Log =
    { id_ : LogId
    , pid_ : ProjectId
    , start_ : Posix
    , end_ : Posix
    }


encoder : Log -> Value
encoder log =
    JE.object
        [ ( "id_", LogId.encoder log.id_ )
        , ( "pid_", ProjectId.encoder log.pid_ )
        , ( "start_", posixEncoder log.start_ )
        , ( "end_", posixEncoder log.end_ )
        ]


decoder : Decoder Log
decoder =
    JD.map4 Log
        (JD.field "id_" LogId.decoder)
        (JD.field "pid_" ProjectId.decoder)
        (JD.field "start_" posixDecoder)
        (JD.field "end_" posixDecoder)


posixDecoder : Decoder Posix
posixDecoder =
    JD.map Time.millisToPosix JD.int


posixEncoder : Posix -> Value
posixEncoder =
    Time.posixToMillis >> JE.int


generator : ProjectId -> Posix -> Posix -> Generator Log
generator projectId_ start_ end_ =
    let
        initHelp : LogId -> Log
        initHelp id_ =
            { id_ = id_
            , pid_ = projectId_
            , start_ = start_
            , end_ = end_
            }
    in
    LogId.generator |> Random.map initHelp


idString : Log -> String
idString =
    id >> LogId.toString


projectId : Log -> ProjectId
projectId =
    .pid_


startDate : Zone -> Log -> Date
startDate zone =
    start >> Date.fromPosix zone


startMillis : Log -> Int
startMillis =
    start >> Time.posixToMillis


endMillis : Log -> Int
endMillis =
    end >> Time.posixToMillis


tracked : Log -> TypedTime
tracked =
    trackedMillis >> toFloat >> TypedTime.milliseconds


sumTracked : List Log -> TypedTime
sumTracked =
    List.foldl (tracked >> TypedTime.add) TypedTime.zero



-- PRIVATE


trackedMillis : Log -> Int
trackedMillis log =
    endMillis log - startMillis log


id : Log -> LogId
id =
    .id_


start : Log -> Posix
start =
    .start_


end : Log -> Posix
end =
    .end_
