module Common.Requests exposing
    ( getOrganization
    , getPackage
    , getPackages
    , getToken
    , postForgottenTokenActionKey
    , postOrganization
    , putOrganization
    , putOrganizationState
    , putOrganizationToken
    )

import Common.AppState exposing (AppState)
import Common.Entities.OrganizationDetail as OrganizationDetail exposing (OrganizationDetail)
import Common.Entities.Package as Package exposing (Package)
import Common.Entities.PackageDetail as PackageDetail exposing (PackageDetail)
import Http
import Json.Decode as D
import Json.Encode as E


postForgottenTokenActionKey :
    { email : String }
    -> AppState
    -> (Result Http.Error () -> msg)
    -> Cmd msg
postForgottenTokenActionKey { email } appState msg =
    let
        body =
            E.object
                [ ( "type", E.string "ForgottenTokenActionKey" )
                , ( "email", E.string email )
                ]
    in
    Http.post
        { url = appState.apiUrl ++ "/action-keys"
        , body = Http.jsonBody body
        , expect = Http.expectWhatever msg
        }


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


putOrganization :
    { name : String
    , description : String
    , email : String
    }
    -> AppState
    -> (Result Http.Error OrganizationDetail -> msg)
    -> Cmd msg
putOrganization data appState msg =
    let
        body =
            E.object
                [ ( "name", E.string data.name )
                , ( "description", E.string data.description )
                , ( "email", E.string data.email )
                ]

        orgId =
            appState.credentials
                |> Maybe.map .organizationId
                |> Maybe.withDefault ""
    in
    Http.request
        { method = "PUT"
        , headers = authHeadersFromAppState appState
        , url = appState.apiUrl ++ "/organizations/" ++ orgId
        , body = Http.jsonBody body
        , expect = Http.expectJson msg OrganizationDetail.decoder
        , timeout = Nothing
        , tracker = Nothing
        }


getOrganization :
    { organizationId : String
    , token : String
    }
    -> AppState
    -> (Result Http.Error OrganizationDetail -> msg)
    -> Cmd msg
getOrganization { organizationId, token } appState msg =
    Http.request
        { method = "GET"
        , headers = authHeaders token
        , url = appState.apiUrl ++ "/organizations/" ++ organizationId
        , body = Http.emptyBody
        , expect = Http.expectJson msg OrganizationDetail.decoder
        , timeout = Nothing
        , tracker = Nothing
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


putOrganizationToken :
    { organizationId : String
    , hash : String
    }
    -> AppState
    -> (Result Http.Error OrganizationDetail -> msg)
    -> Cmd msg
putOrganizationToken data appState msg =
    Http.request
        { method = "PUT"
        , headers = []
        , url = appState.apiUrl ++ "/organizations/" ++ data.organizationId ++ "/token?hash=" ++ data.hash
        , body = Http.emptyBody
        , expect = Http.expectJson msg OrganizationDetail.decoder
        , timeout = Nothing
        , tracker = Nothing
        }


getToken :
    { organizationId : String
    , token : String
    }
    -> AppState
    -> (Result Http.Error String -> msg)
    -> Cmd msg
getToken data appState msg =
    Http.request
        { method = "GET"
        , headers = authHeaders data.token
        , url = appState.apiUrl ++ "/organizations/" ++ data.organizationId
        , body = Http.emptyBody
        , expect = Http.expectJson msg (D.field "token" D.string)
        , timeout = Nothing
        , tracker = Nothing
        }


getPackages : AppState -> (Result Http.Error (List Package) -> msg) -> Cmd msg
getPackages appState msg =
    Http.get
        { url = appState.apiUrl ++ "/packages"
        , expect = Http.expectJson msg (D.list Package.decoder)
        }


getPackage : AppState -> String -> (Result Http.Error PackageDetail -> msg) -> Cmd msg
getPackage appState pkgId msg =
    Http.get
        { url = appState.apiUrl ++ "/packages/" ++ pkgId
        , expect = Http.expectJson msg PackageDetail.decoder
        }


authHeadersFromAppState : AppState -> List Http.Header
authHeadersFromAppState appState =
    case appState.credentials of
        Just credentials ->
            authHeaders credentials.token

        Nothing ->
            []


authHeaders : String -> List Http.Header
authHeaders token =
    [ Http.header "Authorization" <| "Bearer " ++ token ]
