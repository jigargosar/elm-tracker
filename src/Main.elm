module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Random exposing (Generator, Seed)
import Time exposing (Posix)


type alias Project =
    { id : ProjectId
    , title : String
    }


type ProjectId
    = ProjectId String


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
    , seed : Seed
    }


type alias Flags =
    {}


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { pd = Dict.empty
      , seed = Random.initialSeed 0
      }
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



-- Update


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch []



-- View


view : Model -> Html Msg
view model =
    div [ class "measure-wide center ph2 pv2" ]
        [ viewProjectList (Dict.values model.pd)
            |> div [ class "flex flex-column" ]
        ]


viewProjectList : List Project -> List (Html Msg)
viewProjectList =
    let
        vp p =
            div [ class "mv1 pv1" ] [ text p.title ]
    in
    List.map vp



-- Main


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
