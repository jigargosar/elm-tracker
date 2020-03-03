module Log exposing (Log, elapsed, generator)

import LogId exposing (LogId)
import ProjectId exposing (ProjectId)
import Random exposing (Generator)
import Time exposing (Posix)
import TypedTime exposing (TypedTime)


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
        initHelp id =
            { id_ = id
            , pid_ = projectId_
            , start_ = start_
            , end_ = end_
            }
    in
    LogId.generator |> Random.map initHelp


elapsedMillis : Log -> Int
elapsedMillis log =
    endMillis log - startMillis log


elapsed : Log -> TypedTime
elapsed =
    elapsedMillis >> toFloat >> TypedTime.milliseconds


startMillis : Log -> Int
startMillis =
    start >> Time.posixToMillis


endMillis : Log -> Int
endMillis =
    end >> Time.posixToMillis


start : Log -> Posix
start =
    .start_


end : Log -> Posix
end =
    .end_
