module Common.View.ActionButton exposing
    ( button
    , submit
    )

--import Common.Html exposing (fa)

import ActionResult exposing (ActionResult(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import String


{-| Action button invokes a message when clicked. It's appState is defined by
the ActionResult. If the appState is Loading action button is disabled and
a loader is shown instead of action name.
-}
button : ( String, ActionResult a, msg ) -> Html msg
button ( label, result, msg ) =
    actionButtonView [ onClick msg ] label result


submit : ( String, ActionResult a ) -> Html msg
submit ( label, result ) =
    actionButtonView [ type_ "submit" ] label result


actionButtonView : List (Attribute msg) -> String -> ActionResult a -> Html msg
actionButtonView attributes label result =
    let
        buttonContent =
            case result of
                Loading ->
                    span [ class "spinner-border spinner-border-sm" ] []

                --                    text "loading"
                --                    fa "spinner fa-spin"
                _ ->
                    text label

        buttonAttributes =
            [ class "btn btn-primary btn-with-loader", disabled (result == Loading) ] ++ attributes
    in
    Html.button buttonAttributes [ buttonContent ]
