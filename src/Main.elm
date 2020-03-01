module Main exposing (main)

import Dict exposing (Dict)
import Html exposing (Html, button, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Html.Extra exposing (viewMaybe)
import Random exposing (Generator, Seed)
import Task
import Time exposing (Posix)
import TimeTravel.Browser
import TypedTime
import Update.Pipeline exposing (..)



-- ID


idGen : (String -> id) -> Generator id
idGen tag =
    Random.int 0 Random.maxInt |> Random.map (String.fromInt >> tag)



-- ProjectId


type ProjectId
    = ProjectId String


pidToString : ProjectId -> String
pidToString (ProjectId str) =
    str



-- Project


type alias Project =
    { id : ProjectId
    , title : String
    }


projectGen : String -> Generator Project
projectGen title =
    idGen
        (ProjectId
            >> (\id ->
                    { id = id
                    , title = title
                    }
               )
        )



-- LOG ENTRIES


type LogId
    = LogId String


type alias Log =
    { id : LogId
    , pid : ProjectId
    , start : Posix
    , end : Posix
    }


logGen : Activity -> Posix -> Generator Log
logGen activity now =
    let
        initHelp : LogId -> Log
        initHelp id =
            { id = id
            , pid = activity.pid
            , start = activity.start
            , end = now
            }
    in
    idGen (LogId >> initHelp)



-- TRACKED ACTIVITY


type alias Activity =
    { pid : ProjectId
    , start : Posix
    }



-- Model


type alias Model =
    { projectDict : Dict String Project
    , logDict : Dict String Log
    , activity : Maybe Activity
    , now : Posix
    , seed : Seed
    }


getRecentLogs : Model -> List Log
getRecentLogs =
    .logDict >> Dict.values >> List.sortBy (.end >> Time.posixToMillis)


findProject : ProjectId -> ProjectDict -> Maybe Project
findProject projectId =
    Dict.get (pidToString projectId)


getAllProjects : Model -> List Project
getAllProjects =
    .projectDict >> Dict.values


type alias Flags =
    { now : Int
    }


init : Flags -> ( Model, Cmd Msg )
init { now } =
    let
        model : Model
        model =
            { projectDict = Dict.empty
            , logDict = Dict.empty
            , seed = Random.initialSeed 0
            , now = Time.millisToPosix now
            , activity = Nothing
            }
    in
    ( model
        |> insertNewProject "P1"
        |> insertNewProject "P2"
        |> insertNewProject "P3"
    , Cmd.none
    )
        |> andThen startFirstActivity


insertNewProject : String -> Model -> Model
insertNewProject title model =
    case Random.step (projectGen title) model.seed of
        ( project, seed ) ->
            { model
                | activity = Nothing
                , projectDict = insertProject project model.projectDict
                , seed = seed
            }


type alias ProjectDict =
    Dict String Project


insertProject : Project -> ProjectDict -> ProjectDict
insertProject project =
    Dict.insert (pidToString project.id) project


logIdToString : LogId -> String
logIdToString (LogId id) =
    id


type alias LogDict =
    Dict String Log


insertLog : Log -> LogDict -> LogDict
insertLog log =
    Dict.insert (logIdToString log.id) log


startTracking : ProjectId -> Posix -> Model -> Model
startTracking pid posix model =
    case model.activity of
        Just activity ->
            case Random.step (logGen activity model.now) model.seed of
                ( log, seed ) ->
                    { model
                        | activity = Just (Activity pid posix)
                        , logDict = insertLog log model.logDict
                        , seed = seed
                    }

        Nothing ->
            { model
                | activity = Just (Activity pid posix)
            }


stopTracking : Model -> Model
stopTracking model =
    case model.activity of
        Just activity ->
            case Random.step (logGen activity model.now) model.seed of
                ( log, seed ) ->
                    { model
                        | activity = Nothing
                        , logDict = insertLog log model.logDict
                        , seed = seed
                    }

        Nothing ->
            { model | activity = Nothing }



-- Update


type Msg
    = NoOp
    | TrackProjectClicked ProjectId
    | TrackProjectWithNow ProjectId Posix
    | GotNow Posix
    | StopClicked


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        TrackProjectClicked pid ->
            ( model, trackProjectIdCmd pid )

        TrackProjectWithNow projectId start ->
            ( startTracking projectId start model, Cmd.none )

        GotNow now ->
            ( { model | now = now }, Cmd.none )

        StopClicked ->
            ( stopTracking model, Cmd.none )


startFirstActivity : Model -> ( Model, Cmd Msg )
startFirstActivity model =
    case getAllProjects model |> List.head of
        Just p ->
            ( model, trackProjectIdCmd p.id )

        Nothing ->
            ( model, Cmd.none )


trackProjectIdCmd : ProjectId -> Cmd Msg
trackProjectIdCmd pid =
    Task.perform (TrackProjectWithNow pid) Time.now


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every 1000 GotNow
        ]



-- View


view : Model -> Html Msg
view model =
    column [ class "measure-narrow center ph2 pv2" ]
        [ viewProjectList (getAllProjects model)
            |> column []
        , viewMaybe viewActivity (activityView model)
        , row [] [ text "Recent LOGS" ]
        , viewLogList (getRecentLogs model)
            |> column []
        ]


activityView : Model -> Maybe ActivityView
activityView model =
    case model.activity of
        Just activity ->
            case findProject activity.pid model.projectDict of
                Just p ->
                    { pid = activity.pid
                    , title = p.title
                    , start = activity.start
                    , now = model.now
                    }
                        |> Just

                Nothing ->
                    Nothing

        Nothing ->
            Nothing


type alias ActivityView =
    { pid : ProjectId
    , title : String
    , start : Posix
    , now : Posix
    }


viewActivity : ActivityView -> Html Msg
viewActivity vm =
    let
        elapsed =
            posixDiff vm.start vm.now
                |> toFloat
                |> TypedTime.milliseconds
                |> TypedTime.toString TypedTime.Seconds
    in
    column [ class "pv2" ]
        [ row [ class "pv1 f4 " ] [ text "Current Activity" ]
        , row [ class "mb1 justify-between items-baseline" ]
            [ row [] [ text vm.title ]
            , row [] [ text elapsed ]
            ]
        , row [ class "justify-between items-baseline" ]
            [ row [] []
            , button
                [ class "pointer bn pv1 ph2"
                , onClick StopClicked
                ]
                [ text "STOP" ]
            ]
        ]


posixDiff : Posix -> Posix -> Int
posixDiff a b =
    Time.posixToMillis b - Time.posixToMillis a


viewProjectList : List Project -> List (Html Msg)
viewProjectList =
    let
        vp p =
            row [ class "mv1" ]
                [ row [ class "pv1 flex-grow-1" ] [ text p.title ]
                , button
                    [ class "pointer bn pv1 ph2"
                    , onClick <| TrackProjectClicked p.id
                    ]
                    [ text "|>" ]
                ]
    in
    List.map vp


viewLogList : List Log -> List (Html Msg)
viewLogList =
    let
        viewHelp log =
            row [] [ text <| Debug.toString log.pid ]
    in
    List.map viewHelp


row : List (Html.Attribute msg) -> List (Html msg) -> Html msg
row =
    withClass "flex flex-row"


column : List (Html.Attribute msg) -> List (Html msg) -> Html msg
column =
    withClass "flex flex-column"


withClass : String -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
withClass cls a =
    Html.div (class cls :: a)



-- Main


main =
    let
        defaultConfig : TimeTravel.Browser.TimeTravelConfig
        defaultConfig =
            TimeTravel.Browser.defaultConfig
    in
    TimeTravel.Browser.element
        Debug.toString
        Debug.toString
        { defaultConfig | startToLeft = False, startMinimized = True }
        --Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
