module Log exposing (Log, generator)

import LogId exposing (LogId)
import ProjectId exposing (ProjectId)
import Random exposing (Generator)
import Time exposing (Posix)


type alias Log =
    { id_ : LogId
    , pid_ : ProjectId
    , start_ : Posix
    , end_ : Posix
    }


generator : ProjectId -> Posix -> Posix -> Generator Log
generator projectId start end =
    let
        initHelp : LogId -> Log
        initHelp id =
            { id_ = id
            , pid_ = projectId
            , start_ = start
            , end_ = end
            }
    in
    LogId.generator |> Random.map initHelp
