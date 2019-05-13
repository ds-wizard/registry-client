module Common.Requests exposing (postOrganization)

import Common.AppState exposing (AppState)
import Http
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
        { url = appState.apiUrl ++ "/organization"
        , body = Http.jsonBody body
        , expect = Http.expectWhatever msg
        }
