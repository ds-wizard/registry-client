module Common.View.Page exposing (actionResultView)

import ActionResult exposing (ActionResult(..))
import Html exposing (Html, div, h4, p, text)
import Html.Attributes exposing (class)


actionResultView : (a -> Html msg) -> ActionResult a -> Html msg
actionResultView view actionResult =
    case actionResult of
        Success a ->
            view a

        Error err ->
            error err

        _ ->
            loader


loader : Html msg
loader =
    div [ class "text-center animation-fade-in" ]
        [ div [ class "spinner-border spinner-border-lg" ] [] ]


error : String -> Html msg
error err =
    div [ class "alert alert-danger" ]
        [ h4 [ class "alert-heading" ] [ text "Error" ]
        , p [] [ text err ]
        ]
