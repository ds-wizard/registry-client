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
import Common.Entities.ApiError exposing (ApiError(..))
import Common.Entities.OrganizationDetail as OrganizationDetail exposing (OrganizationDetail)
import Common.Entities.Package as Package exposing (Package)
import Common.Entities.PackageDetail as PackageDetail exposing (PackageDetail)
import Http
import Json.Decode as D exposing (Decoder)
import Json.Encode as E


type alias ToMsg a msg =
    Result ApiError a -> msg


postForgottenTokenActionKey :
    { email : String }
    -> AppState
    -> ToMsg () msg
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
        , expect = expectWhatever msg
        }


postOrganization :
    { a
        | organizationId : String
        , name : String
        , description : String
        , email : String
    }
    -> AppState
    -> ToMsg () msg
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
        , expect = expectWhatever msg
        }


putOrganization :
    { name : String
    , description : String
    , email : String
    }
    -> AppState
    -> ToMsg OrganizationDetail msg
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
        , expect = expectJson msg OrganizationDetail.decoder
        , timeout = Nothing
        , tracker = Nothing
        }


getOrganization :
    { organizationId : String
    , token : String
    }
    -> AppState
    -> ToMsg OrganizationDetail msg
    -> Cmd msg
getOrganization { organizationId, token } appState msg =
    Http.request
        { method = "GET"
        , headers = authHeaders token
        , url = appState.apiUrl ++ "/organizations/" ++ organizationId
        , body = Http.emptyBody
        , expect = expectJson msg OrganizationDetail.decoder
        , timeout = Nothing
        , tracker = Nothing
        }


putOrganizationState :
    { organizationId : String
    , hash : String
    , active : Bool
    }
    -> AppState
    -> ToMsg OrganizationDetail msg
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
        , expect = expectJson msg OrganizationDetail.decoder
        , timeout = Nothing
        , tracker = Nothing
        }


putOrganizationToken :
    { organizationId : String
    , hash : String
    }
    -> AppState
    -> ToMsg OrganizationDetail msg
    -> Cmd msg
putOrganizationToken data appState msg =
    Http.request
        { method = "PUT"
        , headers = []
        , url = appState.apiUrl ++ "/organizations/" ++ data.organizationId ++ "/token?hash=" ++ data.hash
        , body = Http.emptyBody
        , expect = expectJson msg OrganizationDetail.decoder
        , timeout = Nothing
        , tracker = Nothing
        }


getToken :
    { organizationId : String
    , token : String
    }
    -> AppState
    -> ToMsg String msg
    -> Cmd msg
getToken data appState msg =
    Http.request
        { method = "GET"
        , headers = authHeaders data.token
        , url = appState.apiUrl ++ "/organizations/" ++ data.organizationId
        , body = Http.emptyBody
        , expect = expectJson msg (D.field "token" D.string)
        , timeout = Nothing
        , tracker = Nothing
        }


getPackages : AppState -> ToMsg (List Package) msg -> Cmd msg
getPackages appState msg =
    Http.get
        { url = appState.apiUrl ++ "/packages"
        , expect = expectJson msg (D.list Package.decoder)
        }


getPackage : AppState -> String -> ToMsg PackageDetail msg -> Cmd msg
getPackage appState pkgId msg =
    Http.get
        { url = appState.apiUrl ++ "/packages/" ++ pkgId
        , expect = expectJson msg PackageDetail.decoder
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


expectJson : ToMsg a msg -> Decoder a -> Http.Expect msg
expectJson toMsg decoder =
    Http.expectStringResponse toMsg <|
        resolve <|
            \string ->
                Result.mapError D.errorToString (D.decodeString decoder string)


expectWhatever : ToMsg () msg -> Http.Expect msg
expectWhatever toMsg =
    Http.expectStringResponse toMsg <|
        resolve <|
            \_ -> Ok ()


resolve : (String -> Result String a) -> Http.Response String -> Result ApiError a
resolve toResult response =
    case response of
        Http.BadUrl_ url ->
            Err (BadUrl url)

        Http.Timeout_ ->
            Err Timeout

        Http.NetworkError_ ->
            Err NetworkError

        Http.BadStatus_ metadata body ->
            Err (BadStatus metadata.statusCode body)

        Http.GoodStatus_ _ body ->
            Result.mapError BadBody (toResult body)
