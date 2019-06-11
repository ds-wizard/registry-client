module Common.Entities.PackageDetail exposing
    ( PackageDetail
    , decoder
    )

import Common.Entities.OrganizationInfo as OrganizationInfo exposing (OrganizationInfo)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Version exposing (Version)


type alias PackageDetail =
    { id : String
    , name : String
    , kmId : String
    , version : Version
    , description : String
    , organization : OrganizationInfo
    , metamodelVersion : Int
    , parentPackageId : Maybe String
    , readme : String
    , versions : List Version
    }


decoder : Decoder PackageDetail
decoder =
    D.succeed PackageDetail
        |> D.required "id" D.string
        |> D.required "name" D.string
        |> D.required "kmId" D.string
        |> D.required "version" Version.decoder
        |> D.required "description" D.string
        |> D.required "organization" OrganizationInfo.decoder
        |> D.required "metamodelVersion" D.int
        |> D.required "parentPackageId" (D.maybe D.string)
        |> D.required "readme" D.string
        |> D.required "versions" (D.list Version.decoder)
