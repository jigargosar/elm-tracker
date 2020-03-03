module Log exposing
    ( Log, generator
    , idString, projectId
    , startDate, endMillis
    , elapsed
    )

{-|

@docs Log, generator

@docs idString, projectId

@docs startDate, endMillis

-}

import Date exposing (Date)
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


endMillis : Log -> Int
endMillis =
    end >> Time.posixToMillis


elapsed : Log -> TypedTime
elapsed =
    elapsedMillis >> toFloat >> TypedTime.milliseconds



-- PRIVATE


startMillis : Log -> Int
startMillis =
    start >> Time.posixToMillis


elapsedMillis : Log -> Int
elapsedMillis log =
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
