module Main exposing (main)

import Browser exposing (Document)
import Html exposing (Html, button, div, img, text)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)


main =
    Browser.document { init = init, update = update, view = view, subscriptions = subscriptions }


type alias Model =
    Int


type Msg
    = Increment
    | Decrement


init : () -> ( Model, Cmd Msg )
init _ =
    ( 0, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( model + 1, Cmd.none )

        Decrement ->
            ( model - 1, Cmd.none )


view : Model -> Document Msg
view model =
    let
        html =
            [ div []
                [ button [ onClick Decrement ] [ text "-" ]
                , div [] [ text (String.fromInt model) ]
                , button [ onClick Increment ] [ text "+" ]
                ]
            ]
    in
    { title = "Elm Webpack Boilerplate"
    , body = html
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
