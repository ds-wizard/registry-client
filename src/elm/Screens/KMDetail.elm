module Screens.KMDetail exposing
    ( Model
    , Msg(..)
    , init
    , update
    , view
    )

import Html exposing (Html, code, div, h1, h4, li, p, text, ul)
import Html.Attributes exposing (class)


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
    div [ class "KMDetail" ]
        [ h1 [ class "ui header" ] [ text "Knowledge Model Name" ]
        , div [ class "ui basic segment" ]
            [ div [ class "ui dividing right rail" ]
                [ h4 [ class "ui header" ] [ text "Knowledge Model ID" ]
                , div [ class "ui grey segment" ] [ text "orgId:kmId:2.1.0" ]
                , h4 [ class "ui header" ] [ text "Published by" ]
                , p [] [ text "My Organization" ]
                , h4 [ class "ui header" ] [ text "Other versions" ]
                , ul [ class "ui list" ]
                    [ li [] [ text "2.0.0" ]
                    , li [] [ text "1.8.3" ]
                    , li [] [ text "1.5.9" ]
                    ]
                , h4 [ class "ui header" ] [ text "Metamodel versions" ]
                , p [] [ text "2, 3, 4" ]
                ]
            , p [] [ text "text" ]
            , p [] [ text "text" ]
            , p [] [ text "text" ]
            , p [] [ text "text" ]
            ]
        ]
