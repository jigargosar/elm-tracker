module Main exposing (main)

import Basics.Extra exposing (flip)
import Date exposing (Date)
import Dict exposing (Dict)
import Html exposing (Html, a, button, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Html.Events.Extra exposing (onClickPreventDefault)
import Html.Extra exposing (viewMaybe)
import Json.Decode as JD
import Json.Encode exposing (Value)
import List.Extra
import Log exposing (Log)
import LogDict exposing (LogDict)
import LogId
import Pivot exposing (Pivot)
import Port
import Project exposing (Project)
import ProjectId exposing (ProjectId)
import Random exposing (Generator, Seed)
import Task
import Time exposing (Posix, Zone)
import TimeTravel.Browser
import TypedTime exposing (TypedTime)
import Update.Pipeline exposing (..)
import Utils exposing (is, propEq)



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
    Dict.get (ProjectId.toString projectId)


findProjectTitled : String -> ProjectDict -> Maybe Project
findProjectTitled title =
    Dict.values >> List.Extra.find (propEq Project.title title)


insertProject : Project -> ProjectDict -> ProjectDict
insertProject project =
    Dict.insert (Project.idString project) project



-- Model


type Tab
    = RecentTab
    | ProjectsTab


type alias Model =
    { projectDict : Dict String Project
    , logDict : Dict String Log
    , activity : Maybe Activity
    , nowForView : Posix
    , here : Zone
    , seed : Seed
    , tabs : Pivot Tab
    }


type alias Flags =
    { now : Int
    , logDict : Value
    }


init : Flags -> ( Model, Cmd Msg )
init { now, logDict } =
    let
        _ =
            LogId.foo

        model : Model
        model =
            { projectDict = Dict.empty
            , logDict =
                case JD.decodeValue LogDict.decoder logDict |> Result.mapError (JD.errorToString >> Debug.log "err") of
                    Err _ ->
                        Debug.todo "implement decoding error for logDict"

                    Ok ld ->
                        ld
            , seed = Random.initialSeed now
            , nowForView = Time.millisToPosix now
            , here = Time.utc
            , activity = Nothing
            , tabs = Pivot.fromCons RecentTab [ ProjectsTab ]
            }
    in
    ( model
    , Time.here |> Task.perform GotHere
    )
        |> andThen
            (flip (List.foldl insertNewProject) mockProjectNames
                >> startActivityTitled currentMockProjectTitle
            )


mockProjectNames =
    [ "Read Elm in action"
    , "Clone Toggle"
    , "Prepare for JP"
    , "Emails"
    , currentMockProjectTitle
    ]


currentMockProjectTitle =
    "Add Mock Logs"



--getAllSortedLogsEntries : Model -> List Log
--getAllSortedLogsEntries =
--    .logDict >> Dict.values >> List.sortBy Log.endMillis
--


getAllProjects : Model -> List Project
getAllProjects =
    .projectDict >> Dict.values


insertNewProject : String -> Model -> Model
insertNewProject title model =
    case Random.step (Project.generator title) model.seed of
        ( project, seed ) ->
            { model
                | activity = Nothing
                , projectDict = insertProject project model.projectDict
                , seed = seed
            }


insertNewLog : Generator Log -> Model -> Model
insertNewLog logGenerator model =
    let
        logDictGenerator : Generator LogDict
        logDictGenerator =
            LogDict.insertGenerator logGenerator model.logDict
    in
    case Random.step logDictGenerator model.seed of
        ( logDict, seed ) ->
            { model | logDict = logDict, seed = seed }


setActivity activity model =
    { model | activity = activity }


logGeneratorForActivity : Posix -> Activity -> Generator Log
logGeneratorForActivity now activity =
    Log.generator activity.pid activity.start now


recordCurrentActivityAndSetTo : Posix -> Maybe Activity -> Model -> Model
recordCurrentActivityAndSetTo now newActivity model =
    (case model.activity |> Maybe.map (logGeneratorForActivity now) of
        Just logGenerator ->
            insertNewLog logGenerator model

        Nothing ->
            model
    )
        |> setActivity newActivity


startTracking : ProjectId -> Posix -> Model -> ( Model, Cmd Msg )
startTracking pid now =
    let
        activity =
            Just (Activity pid now)
    in
    recordCurrentActivityAndSetTo now activity
        >> with cacheLogDict addCmd


cacheLogDict : Model -> Cmd msg
cacheLogDict model =
    Port.cacheLogDict (LogDict.encoder model.logDict)


stopTracking : Posix -> Model -> ( Model, Cmd Msg )
stopTracking now =
    recordCurrentActivityAndSetTo now Nothing
        >> with cacheLogDict addCmd



-- Update


type Msg
    = NoOp
    | OnTabClicked Tab
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

        OnTabClicked tab ->
            ( { model
                | tabs =
                    Pivot.withRollback (Pivot.firstWith (is tab)) model.tabs
              }
            , Cmd.none
            )

        GotHere here ->
            ( { model | here = here }, Cmd.none )

        GotNowForView nowForView ->
            ( { model | nowForView = nowForView }, Cmd.none )

        TrackProjectClicked pid ->
            ( model, trackProjectIdCmd pid )

        StopClicked ->
            ( model, Time.now |> Task.perform StopTrackingWithNow )

        StartTrackWithNow projectId start ->
            startTracking projectId start model

        StopTrackingWithNow now ->
            stopTracking now model


startActivityTitled : String -> Model -> ( Model, Cmd Msg )
startActivityTitled title model =
    case findProjectTitled title model.projectDict of
        Just p ->
            ( model, trackProjectIdCmd (Project.id p) )

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
        , viewTabs model.tabs
        , case Pivot.getC model.tabs of
            RecentTab ->
                --viewTimeLine
                --    model.activity
                --    (Date.fromPosix model.here model.nowForView)
                --    model.here
                --    model.projectDict
                --    (Dict.values model.logDict)
                --column [] [ text "RECENT ENTRIES" ]
                viewRecentLogs model.projectDict (Dict.values model.logDict)

            ProjectsTab ->
                column [] (viewProjectList (getAllProjects model))
        ]


viewTabs : Pivot Tab -> Html Msg
viewTabs tabs =
    let
        toTitle : Tab -> String
        toTitle tab =
            case tab of
                ProjectsTab ->
                    "Projects"

                RecentTab ->
                    "Recent"

        viewTabTitle : Bool -> Tab -> Html Msg
        viewTabTitle isSelected tab =
            row
                [ class "pv1 ph3 flex-auto"
                , class "mid-gray"
                , class "bt br--top br2 bw1 b--transparent"
                , class
                    (if isSelected then
                        "bg-mid-gray white"

                     else
                        "bg-moon-gray"
                    )
                , class "pointer no-selection"
                , onClickPreventDefault (OnTabClicked tab)
                ]
                [ text <| toTitle tab ]

        tabsView =
            tabs
                |> Pivot.mapCS (viewTabTitle True) (viewTabTitle False)
                |> Pivot.toList
    in
    column [ class "pv2 " ] [ row [ class "ph2 bb b--mid-gray justify-around" ] tabsView ]


eqBy f a b =
    f a == f b


viewRecentLogs : ProjectDict -> List Log -> Html Msg
viewRecentLogs pd logs =
    let
        recentGroups : List ( Project, List Log )
        recentGroups =
            logs
                |> List.sortBy (Log.startMillis >> negate)
                |> List.Extra.groupWhile (eqBy Log.projectId)
                |> List.filterMap
                    (\( l, ls ) ->
                        findProject (Log.projectId l) pd
                            |> Maybe.map (\p -> ( p, l :: ls ))
                    )

        viewL ( p, ls ) =
            let
                title =
                    Project.title p

                trackedSum =
                    Log.sumTracked ls
                        |> TypedTime.toString TypedTime.Seconds
            in
            row [ class "" ]
                [ row [ class "flex-grow-1" ] [ text title ]
                , row [] [ text trackedSum ]
                ]
    in
    column [] (List.map viewL recentGroups)



--type alias LogView =
--    { log : Log
--    , project : Project
--    , startDate : Date
--    }
--
--toLogView : Zone -> ProjectDict -> Log -> Maybe LogView
--toLogView zone pd log =
--    Maybe.map
--        (\project ->
--            { log = log
--            , project = project
--            , startDate = Log.startDate zone log
--            }
--        )
--        (findProject (Log.projectId log) pd)
--
--toLogViewList : Zone -> ProjectDict -> List Log -> List LogView
--toLogViewList zone pd =
--    List.filterMap (toLogView zone pd)
--
--
--aggregateLogDurationByProject : List { a | project : Project, log : Log } -> List ( Project, TypedTime )
--aggregateLogDurationByProject =
--    List.Extra.gatherEqualsBy (.project >> Project.id)
--        >> List.map
--            (\( f, r ) ->
--                ( f.project
--                , f :: r |> List.map .log |> Log.sumTracked
--                )
--            )
--        >> List.sortBy (Tuple.second >> TypedTime.toSeconds >> negate)
--
--gatherLogsByDate : Zone -> List Log -> List ( Date, List Log )
--gatherLogsByDate zone =
--    List.Extra.gatherEqualsBy (Log.startDate zone)
--        >> List.map (\( f, r ) -> ( Log.startDate zone f, f :: r ))
--        >> List.sortBy (Tuple.first >> Date.toRataDie >> negate)
--
--
--aggregateLogDurationByProjectId : List Log -> List ( ProjectId, TypedTime )
--aggregateLogDurationByProjectId =
--    List.Extra.gatherEqualsBy Log.projectId
--        >> List.map
--            (\( f, r ) ->
--                ( Log.projectId f
--                , f :: r |> Log.sumTracked
--                )
--            )
--        >> List.sortBy (Tuple.second >> TypedTime.toSeconds >> negate)
--
--gatherLogsByDateThenAggregateLogDurationByProjectId : Zone -> List Log -> List ( Date, List ( ProjectId, TypedTime ) )
--gatherLogsByDateThenAggregateLogDurationByProjectId zone =
--    gatherLogsByDate zone
--        >> List.map (Tuple.mapSecond aggregateLogDurationByProjectId)
--
--viewTimeLine : Maybe Activity -> Date -> Zone -> ProjectDict -> List Log -> Html Msg
--viewTimeLine activity today zone pd logs =
--    let
--        viewProjectEntry : ( ProjectId, TypedTime ) -> Html Msg
--        viewProjectEntry ( projectId, elapsedTT ) =
--            let
--                formattedTime : String
--                formattedTime =
--                    elapsedTT
--                        |> TypedTime.toString TypedTime.Seconds
--
--                projectTitle : String
--                projectTitle =
--                    findProject projectId pd
--                        |> Maybe.Extra.unwrap "<project-title-not-found-error>" Project.title
--            in
--            row [ class "mv1" ]
--                [ row [ class "pv1 mr2 flex-grow-1" ] [ text projectTitle ]
--                , column [ class "pv1 mr2" ] [ text formattedTime ]
--                , button
--                    [ class "pointer bn pv1 ph2"
--                    , onClick <| TrackProjectClicked projectId
--                    ]
--                    [ text "|>" ]
--                ]
--
--        dateToString : Date -> String
--        dateToString date =
--            if date == today then
--                "Today"
--
--            else
--                Date.format "E ddd MMM y" date
--
--        viewDateGroup : ( Date, List ( ProjectId, TypedTime ) ) -> Html Msg
--        viewDateGroup ( date, projectEntryList ) =
--            let
--                filterPE ( pid, _ ) =
--                    case ( date == today, activity ) of
--                        ( True, Just act ) ->
--                            if act.pid == pid then
--                                False
--
--                            else
--                                True
--
--                        _ ->
--                            True
--
--                entryViews =
--                    projectEntryList
--                        |> List.filter filterPE
--                        |> List.map viewProjectEntry
--            in
--            case entryViews of
--                [] ->
--                    nothing
--
--                _ ->
--                    column [ class "pv2" ]
--                        (row [ class "f4" ] [ text (dateToString date) ]
--                            :: entryViews
--                        )
--
--        dateGroupsList =
--            logs
--                |> gatherLogsByDateThenAggregateLogDurationByProjectId zone
--                |> List.map viewDateGroup
--    in
--    column [] dateGroupsList
--


trackedView : Model -> Maybe ActivityView
trackedView model =
    case model.activity of
        Just activity ->
            case findProject activity.pid model.projectDict of
                Just project ->
                    { pid = activity.pid
                    , title = Project.title project
                    , trackedTimeToday =
                        let
                            millisLoggedToday : TypedTime
                            millisLoggedToday =
                                LogDict.logsForProjectIdOnDate model.here
                                    (Date.fromPosix model.here model.nowForView)
                                    activity.pid
                                    model.logDict
                                    |> Log.sumTracked

                            millisTrackedInActivity : TypedTime
                            millisTrackedInActivity =
                                elapsedMillisFromToPosix activity.start model.nowForView
                                    |> toFloat
                                    |> TypedTime.milliseconds
                        in
                        TypedTime.add millisTrackedInActivity millisLoggedToday
                    }
                        |> Just

                Nothing ->
                    Nothing

        Nothing ->
            Nothing


type alias ActivityView =
    { pid : ProjectId
    , title : String
    , trackedTimeToday : TypedTime
    }


viewTracked : ActivityView -> Html Msg
viewTracked vm =
    let
        elapsed : String
        elapsed =
            vm.trackedTimeToday
                |> TypedTime.toString TypedTime.Seconds
    in
    column [ class "pv2" ]
        [ row [ class "pv1 f4 " ] [ text "Tracking" ]
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
        vp : Project -> Html Msg
        vp p =
            row [ class "mv1" ]
                [ row [ class "pv1 flex-grow-1" ] [ text <| Project.title p ]
                , button
                    [ class "pointer bn pv1 ph2"
                    , onClick <| TrackProjectClicked <| Project.id p
                    ]
                    [ text "|>" ]
                ]
    in
    List.map vp


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
