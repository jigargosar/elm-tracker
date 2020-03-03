port module Port exposing (..)

import Json.Encode exposing (Value)


port cacheLogDict : Value -> Cmd msg
