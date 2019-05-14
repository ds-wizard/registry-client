module Common.Entities.Organization exposing
    ( Organization
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias Organization =
    { organizationId : String
    , name : String
    }


decoder : Decoder Organization
decoder =
    D.succeed Organization
        |> D.required "organizationId" D.string
        |> D.required "name" D.string
