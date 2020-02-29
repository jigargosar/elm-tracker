module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, button, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Random exposing (Generator, Seed)
import Task
import Time exposing (Posix)
import Update.Pipeline as UP exposing (..)


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
    , seed : Seed
    }


findProject : ProjectId -> Model -> Maybe Project
findProject projectId model =
    Dict.get (pidToString projectId) model.pd


type alias Flags =
    {}


init : Flags -> ( Model, Cmd Msg )
init _ =
    let
        model : Model
        model =
            { pd = Dict.empty
            , seed = Random.initialSeed 0
            , activity = Nothing
            }
    in
    ( model
        |> insertNewProject "P1"
        |> insertNewProject "P2"
        |> insertNewProject "P3"
    , Cmd.none
    )


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



-- Update


type Msg
    = NoOp
    | TrackProject ProjectId
    | TrackProjectWithNow ProjectId Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update message =
    case message of
        NoOp ->
            save

        TrackProject projectId ->
            save
                >> andAddCmd
                    (getTime (TrackProjectWithNow projectId))

        TrackProjectWithNow projectId start ->
            startActivity projectId start >> save


getTime : (Posix -> msg) -> Cmd msg
getTime func =
    Time.now |> Task.perform func


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch []



-- View


view : Model -> Html Msg
view model =
    column [ class "measure-wide center ph2 pv2" ]
        [ viewProjectList (Dict.values model.pd)
            |> column []
        ]


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


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
