module Common.Requests exposing (getPackages, postOrganization)

import Common.AppState exposing (AppState)
import Common.Entities.Package as Package exposing (Package)
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


getPackages : AppState -> (Result Http.Error (List Package) -> msg) -> Cmd msg
getPackages appState msg =
    Http.get
        { url = appState.apiUrl ++ "/packages/unique"
        , expect = Http.expectJson msg (D.list Package.decoder)
        }
