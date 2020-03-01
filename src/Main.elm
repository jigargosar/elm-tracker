module Main exposing (main)

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


type alias ActivityLog =
    { pid : ProjectId
    , start : Posix
    , end : Posix
    }



-- Model


type alias Model =
    { pd : Dict String Project
    , activity : Maybe Activity
    , now : Posix
    , seed : Seed
    }


findProject : ProjectId -> Model -> Maybe Project
findProject projectId model =
    Dict.get (pidToString projectId) model.pd


type alias Flags =
    { now : Int
    }


init : Flags -> ( Model, Cmd Msg )
init { now } =
    let
        model : Model
        model =
            { pd = Dict.empty
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


using : (a -> a -> b) -> a -> b
using fun model =
    fun model model


with : (a -> b) -> (b -> a -> c) -> a -> c
with extract fun =
    using (fun << extract)


usingJust : (a -> Maybe (a -> b)) -> a -> Maybe b
usingJust func a =
    func a |> Maybe.map ((|>) a)


withJust : (a -> Maybe b) -> (b -> a -> c) -> a -> Maybe c
withJust viewFunc fun =
    usingJust (viewFunc >> Maybe.map fun)


startFirstActivity : Model -> ( Model, Cmd Msg )
startFirstActivity model =
    let
        foo : Model -> ( Model, Cmd Msg )
        foo =
            with
                (.pd >> Dict.values >> List.head)
                (\mp ->
                    case mp of
                        Just p ->
                            addCmd (trackProjectWithNowCmd p)

                        Nothing ->
                            save
                )
    in
    case model.pd |> Dict.values |> List.head of
        Just p ->
            save model
                |> andGetTime (TrackProjectWithNow p.id)

        Nothing ->
            save model


insertNewProject : String -> Model -> Model
insertNewProject title =
    stepRandom (projectGen title) insertProject


insertProject : Project -> Model -> Model
insertProject project =
    mapPd (Dict.insert (pidToString project.id) project)


mapPd func model =
    { model | pd = func model.pd }


stepRandom : Generator a -> (a -> Model -> Model) -> Model -> Model
stepRandom ge func model =
    case Random.step ge model.seed of
        ( a, seed ) ->
            func a { model | seed = seed }


startActivity : Pid -> Posix -> Model -> Model
startActivity pid posix =
    Activity pid posix
        |> Just
        |> setActivity_


setActivity_ a m =
    { m | activity = a }


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

        TrackProject projectId ->
            save >> andGetTime (TrackProjectWithNow projectId)

        TrackProjectWithNow projectId start ->
            startActivity projectId start >> save

        GotNow now ->
            setNow now >> save

        StopClicked ->
            setActivity_ Nothing >> save


getTime : (Posix -> msg) -> Cmd msg
getTime func =
    Time.now |> Task.perform func


trackProjectWithNowCmd : Project -> Cmd Msg
trackProjectWithNowCmd p =
    TrackProjectWithNow p.id
        |> getTime


andGetTime : (Posix -> msg) -> ( a, Cmd msg ) -> ( a, Cmd msg )
andGetTime func =
    Time.now
        |> Task.perform func
        |> andAddCmd


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every 1000 GotNow
        ]



-- View


view : Model -> Html Msg
view model =
    column [ class "measure-narrow center ph2 pv2" ]
        [ viewProjectList (Dict.values model.pd)
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
