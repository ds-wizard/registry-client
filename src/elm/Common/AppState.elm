module Common.AppState exposing
    ( AppState
    , Credentials
    , encodeCredentials
    , init
    , setCredentials
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E


type alias AppState =
    { apiUrl : String
    , valid : Bool
    , credentials : Maybe Credentials
    }


type alias Credentials =
    { organizationId : String
    , token : String
    }


type alias Flags =
    { apiUrl : String
    , credentials : Maybe Credentials
    }


init : D.Value -> AppState
init flagsValue =
    let
        flagsResult =
            D.decodeValue flagsDecoder flagsValue
    in
    case flagsResult of
        Ok flags ->
            AppState flags.apiUrl True flags.credentials

        Err _ ->
            AppState "" False Nothing


flagsDecoder : Decoder Flags
flagsDecoder =
    D.succeed Flags
        |> D.required "apiUrl" D.string
        |> D.required "credentials" (D.maybe credentialsDecoder)


setCredentials : Maybe Credentials -> AppState -> AppState
setCredentials mbCredentials appState =
    { appState | credentials = mbCredentials }


encodeCredentials : Credentials -> E.Value
encodeCredentials credentials =
    E.object
        [ ( "organizationId", E.string credentials.organizationId )
        , ( "token", E.string credentials.token )
        ]


credentialsDecoder : Decoder Credentials
credentialsDecoder =
    D.succeed Credentials
        |> D.required "organizationId" D.string
        |> D.required "token" D.string
