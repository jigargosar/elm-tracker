module Main exposing (main)

import Date exposing (Date)
import Dict exposing (Dict)
import Html exposing (Html, button, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Html.Extra exposing (viewMaybe)
import Random exposing (Generator, Seed)
import Task
import Time exposing (Posix, Zone)
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



-- LOG ID


type LogId
    = LogId String


logIdToString : LogId -> String
logIdToString (LogId id) =
    id



-- LOG ENTRIES


type alias Log =
    { id : LogId
    , pid : ProjectId
    , start : Posix
    , end : Posix
    }


logDurationInMillis : Log -> Int
logDurationInMillis log =
    elapsedMillisFromToPosix log.start log.end


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



-- PROJECT DICT


type alias ProjectDict =
    Dict String Project


findProject : ProjectId -> ProjectDict -> Maybe Project
findProject projectId =
    Dict.get (pidToString projectId)


insertProject : Project -> ProjectDict -> ProjectDict
insertProject project =
    Dict.insert (pidToString project.id) project



-- LOG DICT


type alias LogDict =
    Dict String Log


insertLog : Log -> LogDict -> LogDict
insertLog log =
    Dict.insert (logIdToString log.id) log


logsForProjectIdOnDate : Zone -> Date -> ProjectId -> LogDict -> List Log
logsForProjectIdOnDate zone date projectId =
    Dict.values
        >> List.filter
            (allPass
                [ .pid >> is projectId
                , .start
                    >> Date.fromPosix zone
                    >> Date.isBetween date date
                ]
            )


allPass : List (a -> Bool) -> a -> Bool
allPass list a =
    List.all ((|>) a) list


is =
    (==)



-- Model


type alias Model =
    { projectDict : Dict String Project
    , logDict : Dict String Log
    , activity : Maybe Activity
    , nowForView : Posix
    , here : Zone
    , seed : Seed
    }


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
            , seed = Random.initialSeed now
            , nowForView = Time.millisToPosix now
            , here = Time.utc
            , activity = Nothing
            }
    in
    ( model
    , Time.here |> Task.perform GotHere
    )
        |> andThen
            (insertNewProject "P1"
                >> insertNewProject "P2"
                >> insertNewProject "P3"
                >> startFirstActivity
            )


getAllSortedLogsEntries : Model -> List Log
getAllSortedLogsEntries =
    .logDict >> Dict.values >> List.sortBy (.end >> Time.posixToMillis)


getAllProjects : Model -> List Project
getAllProjects =
    .projectDict >> Dict.values


insertNewProject : String -> Model -> Model
insertNewProject title model =
    case Random.step (projectGen title) model.seed of
        ( project, seed ) ->
            { model
                | activity = Nothing
                , projectDict = insertProject project model.projectDict
                , seed = seed
            }


startTracking : ProjectId -> Posix -> Model -> Model
startTracking pid now model =
    case model.activity of
        Just activity ->
            case Random.step (logGen activity now) model.seed of
                ( log, seed ) ->
                    { model
                        | activity = Just (Activity pid now)
                        , logDict = insertLog log model.logDict
                        , seed = seed
                    }

        Nothing ->
            { model
                | activity = Just (Activity pid now)
            }


stopTracking : Posix -> Model -> Model
stopTracking now model =
    case model.activity of
        Just activity ->
            case Random.step (logGen activity now) model.seed of
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
    | GotNowForView Posix
    | GotHere Zone
    | TrackProjectClicked ProjectId
    | StopClicked
    | StartTrackWithNow ProjectId Posix
    | StopTrackingWithNow Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        GotHere here ->
            ( { model | here = here }, Cmd.none )

        GotNowForView nowForView ->
            ( { model | nowForView = nowForView }, Cmd.none )

        TrackProjectClicked pid ->
            ( model, trackProjectIdCmd pid )

        StartTrackWithNow projectId start ->
            ( startTracking projectId start model, Cmd.none )

        StopTrackingWithNow now ->
            ( stopTracking now model, Cmd.none )

        StopClicked ->
            ( model, Time.now |> Task.perform StopTrackingWithNow )


startFirstActivity : Model -> ( Model, Cmd Msg )
startFirstActivity model =
    case getAllProjects model |> List.head of
        Just p ->
            ( model, trackProjectIdCmd p.id )

        Nothing ->
            ( model, Cmd.none )


trackProjectIdCmd : ProjectId -> Cmd Msg
trackProjectIdCmd pid =
    Task.perform (StartTrackWithNow pid) Time.now


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every 1000 GotNowForView
        ]



-- View


view : Model -> Html Msg
view model =
    column [ class "measure-narrow center ph2 pv2" ]
        [ viewMaybe viewTracked (trackedView model)
        , viewProjectList (getAllProjects model)
            |> column []
        , viewDebugList "DEBUG: Log Duration"
            (getAllSortedLogsEntries model
                |> List.map logDurationInMillis
            )
        , viewDebugList "DEBUG: ALL PROJECTS" (getAllProjects model)
        , viewDebugList "DEBUG: ALL LOG ENTRIES" (getAllSortedLogsEntries model)
        ]


trackedView : Model -> Maybe ActivityView
trackedView model =
    case model.activity of
        Just activity ->
            case findProject activity.pid model.projectDict of
                Just p ->
                    { pid = activity.pid
                    , title = p.title
                    , totalMillisToday =
                        let
                            millisLoggedToday =
                                logsForProjectIdOnDate model.here
                                    (Date.fromPosix model.here model.nowForView)
                                    activity.pid
                                    model.logDict
                                    |> List.map logDurationInMillis
                                    |> List.sum
                        in
                        elapsedMillisFromToPosix activity.start model.nowForView
                            |> (+) millisLoggedToday
                    }
                        |> Just

                Nothing ->
                    Nothing

        Nothing ->
            Nothing


type alias ActivityView =
    { pid : ProjectId
    , title : String
    , totalMillisToday : Int
    }


viewTracked : ActivityView -> Html Msg
viewTracked vm =
    let
        elapsed : String
        elapsed =
            vm.totalMillisToday
                |> toFloat
                |> TypedTime.milliseconds
                |> TypedTime.toString TypedTime.Seconds
    in
    column [ class "pv2" ]
        [ row [ class "pv1 f4 " ] [ text "Tracking Today" ]
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


elapsedMillisFromToPosix : Posix -> Posix -> Int
elapsedMillisFromToPosix a b =
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


viewDebugList : String -> List a -> Html msg
viewDebugList title list =
    column [ class "pv2" ]
        (row [ class "pv1 f4" ] [ text title ]
            :: viewDebugListItems list
        )


viewDebugListItems : List a -> List (Html msg)
viewDebugListItems =
    let
        viewDebugListItem item =
            row [] [ text <| Debug.toString item ]
    in
    List.map viewDebugListItem


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
