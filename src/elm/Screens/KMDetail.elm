module Screens.KMDetail exposing
    ( Model
    , Msg(..)
    , init
    , update
    , view
    )

import Html exposing (Html, div, h5, li, p, text, ul)
import Html.Attributes exposing (class)
import Markdown


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
        [ div [ class "row" ]
            [ div [ class "col-12 col-md-8" ]
                [ Markdown.toHtml [] readme ]
            , div [ class "col-12 col-md-4" ]
                [ h5 [] [ text "Knowledge Model ID" ]
                , p [] [ text "orgId:kmId:latest" ]
                , h5 [] [ text "Published by" ]
                , p [] [ text "My Organization" ]
                , h5 [] [ text "Other versions" ]
                , ul []
                    [ li [] [ text "2.0.0" ]
                    , li [] [ text "1.8.3" ]
                    , li [] [ text "1.5.9" ]
                    ]
                , h5 [] [ text "Supported metamodel" ]
                , p [] [ text "Since 2" ]
                ]
            ]
        ]


readme =
    """
# Knowledge Model

This is a knowledge model that can be used for:

- Data Stewardship
- Something Else

This font is **bold** and *italic*.

We can even have [link](http://example.com).
"""
