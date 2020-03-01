module Main exposing (main)

import Basics.Extra exposing (flip, swap)
import Browser
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
import Update.Pipeline as U exposing (..)


type alias Project =
    { id : ProjectId
    , title : String
    }


type ProjectId
    = ProjectId String


type alias Pid =
    ProjectId


pidToString : ProjectId -> String
pidToString (ProjectId str) =
    str


idGen : (String -> id) -> Generator id
idGen tag =
    Random.int 0 Random.maxInt |> Random.map (String.fromInt >> tag)


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


type alias Activity =
    { pid : ProjectId
    , start : Posix
    }



-- ACTIVITY LOG


type LogId
    = ActivityLogId String


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
    idGen (ActivityLogId >> initHelp)



-- Model


type alias Model =
    { pd : Dict String Project
    , activity : Maybe Activity
    , logD : Dict String Log
    , now : Posix
    , seed : Seed
    }


findProject : ProjectId -> Model -> Maybe Project
findProject projectId model =
    Dict.get (pidToString projectId) model.pd


findFirstProject : Model -> Maybe Project
findFirstProject =
    getAllProjects >> List.head


getAllProjects : Model -> List Project
getAllProjects =
    .pd >> Dict.values


type alias Flags =
    { now : Int
    }


init : Flags -> ( Model, Cmd Msg )
init { now } =
    let
        model : Model
        model =
            { pd = Dict.empty
            , logD = Dict.empty
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
        |> U.andThen startFirstActivity


addMaybeCmd : Maybe (Cmd msg) -> a -> ( a, Cmd msg )
addMaybeCmd =
    Maybe.withDefault Cmd.none >> addCmd


startFirstActivity : Model -> ( Model, Cmd Msg )
startFirstActivity =
    with findFirstProject
        (Maybe.map trackProjectCmd >> addMaybeCmd)


insertNewProject : String -> Model -> Model
insertNewProject title =
    stepRandom insertProject (projectGen title)


insertProject : Project -> Model -> Model
insertProject project =
    mapPd (Dict.insert (pidToString project.id) project)


mapPd func model =
    { model | pd = func model.pd }


stepRandom : (a -> { has | seed : Seed } -> b) -> Generator a -> { has | seed : Seed } -> b
stepRandom func ge hasSeed =
    case Random.step ge hasSeed.seed of
        ( a, seed ) ->
            func a { hasSeed | seed = seed }


insertNewLog : Log -> Model -> Model
insertNewLog al =
    mapAd (Dict.insert (alIdToString al.id) al)


alIdToString : LogId -> String
alIdToString (ActivityLogId id) =
    id


mapAd : (Dict String Log -> Dict String Log) -> Model -> Model
mapAd func model =
    { model | logD = func model.logD }


logActivity : Activity -> Model -> Model
logActivity activity =
    with (.now >> logGen activity) (stepRandom insertNewLog)


startActivity : Pid -> Posix -> Model -> Model
startActivity pid posix =
    with .activity
        (\ma model ->
            let
                _ =
                    Activity pid posix
                        |> Just

                _ =
                    (case ma of
                        Just running ->
                            logActivity running

                        Nothing ->
                            identity
                    )
                        >> setActivity (Activity pid posix)
            in
            model
        )


setActivity_ : Maybe Activity -> Model -> Model
setActivity_ a m =
    { m | activity = a }


setActivity =
    Just >> setActivity_


setNow now m =
    { m | now = now }



-- Update


type Msg
    = NoOp
    | TrackProject ProjectId
    | TrackProjectWithNow ProjectId Posix
    | GotNow Posix
    | StopClicked


update : Msg -> Model -> ( Model, Cmd Msg )
update message =
    case message of
        NoOp ->
            save

        TrackProject pid ->
            addCmd (trackProjectIdCmd pid)

        TrackProjectWithNow projectId start ->
            startActivity projectId start >> save

        GotNow now ->
            setNow now >> save

        StopClicked ->
            setActivity_ Nothing >> save


performGetTime : (Posix -> msg) -> Cmd msg
performGetTime func =
    Task.perform func Time.now


trackProjectCmd : Project -> Cmd Msg
trackProjectCmd =
    .id >> trackProjectIdCmd


trackProjectIdCmd : ProjectId -> Cmd Msg
trackProjectIdCmd =
    TrackProjectWithNow >> performGetTime


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
        ]


activityView : Model -> Maybe ActivityView
activityView model =
    case model.activity of
        Just activity ->
            case findProject activity.pid model of
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
                    , onClick <| TrackProject p.id
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
