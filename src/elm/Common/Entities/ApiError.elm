module Common.Entities.ApiError exposing (ApiError(..), decode, toActionResult)

import ActionResult exposing (ActionResult(..))
import Common.Entities.ServerError as ServerError exposing (ServerError)
import Json.Decode as D


type ApiError
    = BadUrl String
    | Timeout
    | NetworkError
    | BadStatus Int String
    | BadBody String


decode : ApiError -> Maybe ServerError
decode error =
    case error of
        BadStatus _ response ->
            case D.decodeString ServerError.decoder response of
                Ok err ->
                    Just err

                _ ->
                    Nothing

        _ ->
            Nothing


toActionResult : String -> ApiError -> ActionResult a
toActionResult defaultMessage error =
    case decode error of
        Just err ->
            if String.isEmpty err.message then
                Error defaultMessage

            else
                Error err.message

        Nothing ->
            Error defaultMessage
