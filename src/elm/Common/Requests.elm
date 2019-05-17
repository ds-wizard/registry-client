module Common.Requests exposing (getPackage, getPackages, postOrganization, putOrganizationState)

import Common.AppState exposing (AppState)
import Common.Entities.OrganizationDetail as OrganizationDetail exposing (OrganizationDetail)
import Common.Entities.Package as Package exposing (Package)
import Common.Entities.PackageDetail as PackageDetail exposing (PackageDetail)
import Http
import Json.Decode as D
import Json.Encode as E


postOrganization :
    { organizationId : String
    , name : String
    , description : String
    , email : String
    }
    -> AppState
    -> (Result Http.Error () -> msg)
    -> Cmd msg
postOrganization organization appState msg =
    let
        body =
            E.object
                [ ( "organizationId", E.string organization.organizationId )
                , ( "name", E.string organization.name )
                , ( "description", E.string organization.description )
                , ( "email", E.string organization.email )
                ]
    in
    Http.post
        { url = appState.apiUrl ++ "/organizations"
        , body = Http.jsonBody body
        , expect = Http.expectWhatever msg
        }


putOrganizationState :
    { organizationId : String
    , hash : String
    , active : Bool
    }
    -> AppState
    -> (Result Http.Error OrganizationDetail -> msg)
    -> Cmd msg
putOrganizationState data appState msg =
    let
        body =
            E.object [ ( "active", E.bool data.active ) ]
    in
    Http.request
        { method = "PUT"
        , headers = []
        , url = appState.apiUrl ++ "/organizations/" ++ data.organizationId ++ "/state?hash=" ++ data.hash
        , body = Http.jsonBody body
        , expect = Http.expectJson msg OrganizationDetail.decoder
        , timeout = Nothing
        , tracker = Nothing
        }


getPackages : AppState -> (Result Http.Error (List Package) -> msg) -> Cmd msg
getPackages appState msg =
    Http.get
        { url = appState.apiUrl ++ "/packages/unique"
        , expect = Http.expectJson msg (D.list Package.decoder)
        }


getPackage : AppState -> String -> (Result Http.Error PackageDetail -> msg) -> Cmd msg
getPackage appState pkgId msg =
    Http.get
        { url = appState.apiUrl ++ "/packages/" ++ pkgId
        , expect = Http.expectJson msg PackageDetail.decoder
        }
