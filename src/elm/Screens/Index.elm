module Screens.Index exposing
    ( Model
    , Msg(..)
    , init
    , update
    , view
    )

import Html exposing (Html, a, div, h1, h5, p, small, span, text)
import Html.Attributes exposing (class, href)


type alias Model =
    {}


type Msg
    = NoOp


init : Model
init =
    {}


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ div [ class "list-group list-group-flush" ]
            [ viewItem, viewItem, viewItem, viewItem ]
        ]


viewItem : Html Msg
viewItem =
    div [ class "list-group-item flex-column align-items-start" ]
        [ div [ class "d-flex justify-content-between" ]
            [ h5 [ class "mb-1" ]
                [ a [ href "/km/orgId/kmId/latest" ] [ text "Knowledge Model Name" ]
                ]
            , small [] [ text "Organization Name" ]
            ]
        , p [] [ text "This is a sample knowledge model with a couple of chapters." ]
        ]
