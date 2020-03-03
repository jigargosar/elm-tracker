module Main exposing (main)

import Date exposing (Date)
import Dict exposing (Dict)
import Html exposing (Html, a, button, option, select, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Html.Events.Extra exposing (onClickPreventDefault)
import Html.Extra exposing (viewMaybe)
import Json.Decode as JD
import Json.Encode as JE exposing (Value)
import List.Extra
import Log exposing (Log)
import LogDict exposing (LogDict)
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
    , projectDict : Value
    }


init : Flags -> ( Model, Cmd Msg )
init { now, logDict, projectDict } =
    let
        model : Model
        model =
            { projectDict =
                case
                    JD.decodeValue (JD.dict Project.decoder) projectDict
                        |> Result.mapError (JD.errorToString >> Debug.log "err")
                of
                    Err _ ->
                        Debug.todo "implement decoding error for projectDict"

                    Ok dict ->
                        dict
            , logDict =
                case
                    JD.decodeValue LogDict.decoder logDict
                        |> Result.mapError (JD.errorToString >> Debug.log "err")
                of
                    Err _ ->
                        Debug.todo "implement decoding error for logDict"

                    Ok dict ->
                        dict
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
        |> (andThenIf (Dict.isEmpty model.projectDict) insertMockProjects
                >> andThen (startActivityTitled currentMockProjectTitle)
           )


insertMockProjects : Model -> ( Model, Cmd Msg )
insertMockProjects =
    sequence (List.map insertNewProject mockProjectNames)


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


insertNewProject : String -> Model -> ( Model, Cmd Msg )
insertNewProject title model =
    case Random.step (Project.generator title) model.seed of
        ( project, seed ) ->
            { model
                | activity = Nothing
                , projectDict = insertProject project model.projectDict
                , seed = seed
            }
                |> with cacheProjectDict addCmd


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


cacheProjectDict : Model -> Cmd msg
cacheProjectDict model =
    Port.cacheProjectDict (JE.dict identity Project.encoder model.projectDict)


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
        , viewTracked2 (toActivityView2 model.nowForView model.projectDict model.activity)
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


type alias ActivityView2 =
    { pid : ProjectId
    , title : String
    , elapsed : TypedTime
    }


toActivityView2 : Posix -> ProjectDict -> Maybe Activity -> Maybe ActivityView2
toActivityView2 now pd activity =
    case activity of
        Just { pid, start } ->
            case findProject pid pd of
                Just p ->
                    Just
                        { pid = pid
                        , title = Project.title p
                        , elapsed =
                            elapsedMillisFromToPosix start now
                                |> toFloat
                                |> TypedTime.milliseconds
                        }

                Nothing ->
                    Nothing

        Nothing ->
            Nothing


type alias ActivityView =
    { pid : ProjectId
    , title : String
    , trackedTimeToday : TypedTime
    }


viewTracked2 : Maybe ActivityView2 -> Html Msg
viewTracked2 maybeV =
    case maybeV of
        Nothing ->
            let
                o1 title =
                    option [] [ text title ]

                titles =
                    mockProjectNames
            in
            column [ class "pv2" ]
                [ column [] [ select [] (List.map o1 titles) ]
                , row [ class "pv1" ]
                    [ row [ class "flex-auto" ] []
                    , row [ class "" ] [ btn1 "START" ]
                    ]
                ]

        Just v ->
            let
                o1 title =
                    option [] [ text title ]

                titles =
                    mockProjectNames
            in
            column [ class "pv2" ]
                [ column [] [ select [] (List.map o1 titles) ]
                , row [ class "pv1" ]
                    [ row [ class "flex-auto" ] []
                    , row [ class "pv1 mr2" ] [ text "00:02::11" ]
                    , row [ class "" ] [ btn1 "STOP" ]
                    ]
                ]


btn1 : String -> Html Msg
btn1 =
    btn2 NoOp


btn2 : Msg -> String -> Html Msg
btn2 msg title =
    button
        [ class "pointer bn pv1 ph2"
        , onClick msg
        ]
        [ text title ]


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
