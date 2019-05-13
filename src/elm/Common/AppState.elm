module Common.AppState exposing (AppState, init)


type alias AppState =
    { apiUrl : String }


init : String -> AppState
init =
    AppState
