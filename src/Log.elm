module Log exposing
    ( Log, generator
    , idString, projectId
    , startDate, endMillis
    , sumTracked
    , encoder, startMillis
    )

{-|

@docs Log, generator

@docs idString, projectId

@docs startDate, endMillis

@docs sumTracked

-}

import Date exposing (Date)
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
        , ( "start_", JE.int <| startMillis log )
        , ( "end_", JE.int <| endMillis log )
        ]


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
