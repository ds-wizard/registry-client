module Main exposing (main)

import Browser exposing (Document)
import Html exposing (Html, a, div, img, text)
import Html.Attributes exposing (class, href, src)


main =
    Browser.document { init = init, update = update, view = view, subscriptions = subscriptions }


type alias Model =
    Int


type Msg
    = NoOp


init : () -> ( Model, Cmd Msg )
init _ =
    ( 0, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Model -> Document Msg
view model =
    let
        html =
            [ header
            , div [ class "ui main text container" ]
                []
            ]
    in
    { title = "Elm Webpack Boilerplate"
    , body = html
    }


header : Html Msg
header =
    div [ class "ui fixed inverted menu" ]
        [ div [ class "ui container" ]
            [ a [ href "#", class "header item" ]
                [ img [ class "logo", src "/img/logo.svg" ] []
                , text "Registry"
                ]
            , div [ class "right menu" ]
                [ a [ href "#", class " item" ]
                    [ text "Log in" ]
                , a [ href "#", class " item" ]
                    [ text "Sign up" ]
                ]
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
