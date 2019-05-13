module Screens.Login exposing
    ( Model
    , Msg(..)
    , init
    , update
    , view
    )

import Html exposing (Html, div, text)


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
    div [] [ text "Login" ]
