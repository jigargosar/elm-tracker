module LogDict exposing (LogDict, decoder, encoder, insertGenerator, logsForProjectIdOnDate)

import Basics.Extra exposing (flip)
import Date exposing (Date)
import Dict exposing (Dict)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import Log exposing (Log)
import ProjectId exposing (ProjectId)
import Random exposing (Generator)
import Time exposing (Zone)
import Utils exposing (allPass, is, propEq, propSatisfies)


type alias LogDict =
    Dict String Log


insertLog : Log -> LogDict -> LogDict
insertLog log =
    Dict.insert (Log.idString log) log


insertLogIn : LogDict -> Log -> LogDict
insertLogIn =
    flip insertLog


insertGenerator : Generator Log -> LogDict -> Generator LogDict
insertGenerator logGen logDict =
    Random.map (insertLogIn logDict) logGen


logsForProjectIdOnDate : Zone -> Date -> ProjectId -> LogDict -> List Log
logsForProjectIdOnDate zone date projectId =
    Dict.values
        >> List.filter
            (allPass
                [ propEq Log.projectId projectId
                , propSatisfies (Log.startDate zone) (Date.isBetween date date)
                ]
            )


encoder : LogDict -> Value
encoder =
    JE.dict identity Log.encoder


decoder : Decoder LogDict
decoder =
    JD.dict Log.decoder
