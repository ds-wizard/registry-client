module Screens.Index exposing
    ( Model
    , Msg(..)
    , init
    , update
    , view
    )

import Html exposing (Html, a, div, h1, p, span, text)
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
        [ h1 [ class "ui header" ] [ text "Knowledge Models" ]
        , div [ class "ui relaxed divided items" ]
            [ viewItem, viewItem, viewItem, viewItem ]
        ]


viewItem : Html Msg
viewItem =
    div [ class "item" ]
        [ div [ class "content" ]
            [ a [ href "/km/orgId/kmId", class "header" ] [ text "Knowledge Model Name" ]
            , div [ class "meta" ]
                [ span [] [ text "Organization Name" ] ]
            , div [ class "description" ]
                [ p [] [ text "This is a sample knowledge model with a couple of chapters." ]
                ]
            ]
        ]
