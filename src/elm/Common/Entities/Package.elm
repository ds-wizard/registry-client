module Common.Entities.Package exposing
    ( Package
    , decoder
    )

import Common.Entities.Organization as Organization exposing (Organization)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias Package =
    { id : String
    , name : String
    , kmId : String
    , version : String
    , description : String
    , organization : Organization
    }


decoder : Decoder Package
decoder =
    D.succeed Package
        |> D.required "id" D.string
        |> D.required "name" D.string
        |> D.required "kmId" D.string
        |> D.required "version" D.string
        |> D.required "description" D.string
        |> D.required "organization" Organization.decoder
