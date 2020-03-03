module Project exposing (Project, generator, id, idString, title)

import ProjectId exposing (ProjectId)
import Random exposing (Generator)


type alias Project =
    { id_ : ProjectId
    , title_ : String
    }


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
