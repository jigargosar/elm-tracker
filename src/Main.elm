module Main exposing (main)

import Date exposing (Date)
import Dict exposing (Dict)
import Html exposing (Html, a, button, text)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onClick)
import Html.Events.Extra exposing (onClickPreventDefault)
import Html.Extra exposing (nothing, viewMaybe)
import List.Extra
import Log exposing (Log)
import Maybe.Extra
import Project exposing (Project)
import ProjectId exposing (ProjectId)
import Random exposing (Generator, Seed)
import Task
import Time exposing (Posix, Zone)
import TimeTravel.Browser
import TypedTime exposing (TypedTime)
import Update.Pipeline exposing (..)



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


insertProject : Project -> ProjectDict -> ProjectDict
insertProject project =
    Dict.insert (Project.idString project) project



-- LOG DICT


type alias LogDict =
    Dict String Log


insertLog : Log -> LogDict -> LogDict
insertLog log =
    Dict.insert (Log.idString log) log


logsForProjectIdOnDate : Zone -> Date -> ProjectId -> LogDict -> List Log
logsForProjectIdOnDate zone date projectId =
    Dict.values
        >> List.filter
            (allPass
                [ Log.projectId >> is projectId
                , Log.startDate zone >> Date.isBetween date date
                ]
            )


allPass : List (a -> Bool) -> a -> Bool
allPass list a =
    List.all ((|>) a) list


is =
    (==)



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
    , tab : Tab
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
            , tab = RecentTab
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
    .logDict >> Dict.values >> List.sortBy Log.endMillis


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


startTracking : ProjectId -> Posix -> Model -> Model
startTracking pid now model =
    case model.activity of
        Just activity ->
            case Random.step (Log.generator activity.pid activity.start now) model.seed of
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
            case Random.step (Log.generator activity.pid activity.start now) model.seed of
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
    | OnNavLinkClicked Tab
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

        OnNavLinkClicked tab ->
            ( { model | tab = tab }, Cmd.none )

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
        [ viewNavHeader model.tab
        , viewMaybe viewTracked (trackedView model)
        , case model.tab of
            RecentTab ->
                --viewTimeLine
                --    model.activity
                --    (Date.fromPosix model.here model.nowForView)
                --    model.here
                --    model.projectDict
                --    (Dict.values model.logDict)
                column [] [ text "RECENT ENTRIES" ]

            ProjectsTab ->
                column [] (viewProjectList (getAllProjects model))
        ]


viewNavHeader currentTab =
    let
        toClassString tab =
            if tab == currentTab then
                "blue no-underline"

            else
                "black"

        toTitle : Tab -> String
        toTitle tab =
            case tab of
                ProjectsTab ->
                    "ProjectListRoute"

                RecentTab ->
                    "TimelineRoute"

        link tab =
            a
                [ class "mr2"
                , class <| toClassString tab
                , href "/projects"
                , onClickPreventDefault (OnNavLinkClicked tab)
                ]
                [ text <| toTitle tab ]

        links =
            [ RecentTab, ProjectsTab ]
                |> List.map link
    in
    row [ class "pv2" ] links


type alias LogView =
    { log : Log
    , project : Project
    , startDate : Date
    }


toLogView : Zone -> ProjectDict -> Log -> Maybe LogView
toLogView zone pd log =
    Maybe.map
        (\project ->
            { log = log
            , project = project
            , startDate = Log.startDate zone log
            }
        )
        (findProject (Log.projectId log) pd)


toLogViewList : Zone -> ProjectDict -> List Log -> List LogView
toLogViewList zone pd =
    List.filterMap (toLogView zone pd)


aggregateLogDurationByProject : List { a | project : Project, log : Log } -> List ( Project, TypedTime )
aggregateLogDurationByProject =
    List.Extra.gatherEqualsBy (.project >> Project.id)
        >> List.map
            (\( f, r ) ->
                ( f.project
                , f :: r |> List.map .log |> Log.sumTracked
                )
            )
        >> List.sortBy (Tuple.second >> TypedTime.toSeconds >> negate)


gatherLogsByDate : Zone -> List Log -> List ( Date, List Log )
gatherLogsByDate zone =
    List.Extra.gatherEqualsBy (Log.startDate zone)
        >> List.map (\( f, r ) -> ( Log.startDate zone f, f :: r ))
        >> List.sortBy (Tuple.first >> Date.toRataDie >> negate)


aggregateLogDurationByProjectId : List Log -> List ( ProjectId, TypedTime )
aggregateLogDurationByProjectId =
    List.Extra.gatherEqualsBy Log.projectId
        >> List.map
            (\( f, r ) ->
                ( Log.projectId f
                , f :: r |> Log.sumTracked
                )
            )
        >> List.sortBy (Tuple.second >> TypedTime.toSeconds >> negate)


gatherLogsByDateThenAggregateLogDurationByProjectId : Zone -> List Log -> List ( Date, List ( ProjectId, TypedTime ) )
gatherLogsByDateThenAggregateLogDurationByProjectId zone =
    gatherLogsByDate zone
        >> List.map (Tuple.mapSecond aggregateLogDurationByProjectId)



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
                                logsForProjectIdOnDate model.here
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
