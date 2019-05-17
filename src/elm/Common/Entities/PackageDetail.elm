module Common.Entities.PackageDetail exposing
    ( PackageDetail
    , decoder
    )

import Common.Entities.Organization as Organization exposing (Organization)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias PackageDetail =
    { id : String
    , name : String
    , kmId : String
    , version : String
    , description : String
    , organization : Organization
    , metamodelVersion : Int
    , parentPackageId : Maybe String
    , readme : String
    , versions : List String
    }


decoder : Decoder PackageDetail
decoder =
    D.succeed PackageDetail
        |> D.required "id" D.string
        |> D.required "name" D.string
        |> D.required "kmId" D.string
        |> D.required "version" D.string
        |> D.required "description" D.string
        |> D.required "organization" Organization.decoder
        |> D.required "metamodelVersion" D.int
        |> D.required "parentPackageId" (D.maybe D.string)
        |> D.required "readme" D.string
        |> D.required "versions" (D.list D.string)
