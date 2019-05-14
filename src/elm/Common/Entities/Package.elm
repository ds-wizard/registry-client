module Common.Entities.Package exposing
    ( Package
    , decoder
    )

import Common.Entities.Organization as Organization exposing (Organization)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias Package =
    { name : String
    , kmId : String
    , version : String
    , shortDescription : String
    , organization : Organization
    }


decoder : Decoder Package
decoder =
    D.succeed Package
        |> D.required "name" D.string
        |> D.required "kmId" D.string
        |> D.required "version" D.string
        |> D.required "shortDescription" D.string
        |> D.required "organization" Organization.decoder
