module Project exposing (Project, decoder, encoder, generator, id, idString, title)

import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import ProjectId exposing (ProjectId)
import Random exposing (Generator)


type alias Project =
    { id_ : ProjectId
    , title_ : String
    }


decoder : Decoder Project
decoder =
    JD.map2 Project
        (JD.field "id_" ProjectId.decoder)
        (JD.field "title_" JD.string)


encoder : Project -> Value
encoder project =
    JE.object
        [ ( "id_", ProjectId.encoder project.id_ )
        , ( "title_", JE.string project.title_ )
        ]


generator : String -> Generator Project
generator title_ =
    ProjectId.generator
        |> Random.map
            (\id_ ->
                { id_ = id_
                , title_ = title_
                }
            )


title : Project -> String
title =
    .title_


id : Project -> ProjectId
id =
    .id_


idString : Project -> String
idString =
    id >> ProjectId.toString
