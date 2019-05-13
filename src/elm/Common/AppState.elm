module Common.AppState exposing (AppState, init)

import Json.Decode as D


type alias AppState =
    { apiUrl : String
    , valid : Bool
    }


init : D.Value -> AppState
init flags =
    let
        apiUrlResult =
            D.decodeValue (D.field "apiUrl" D.string) flags
    in
    case apiUrlResult of
        Ok apiUrl ->
            AppState apiUrl True

        Err _ ->
            AppState "" False
