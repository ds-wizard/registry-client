module Screens.Index exposing
    ( Model
    , Msg(..)
    , init
    , initEmpty
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Common.AppState exposing (AppState)
import Common.Entities.Package exposing (Package)
import Common.Requests as Requests
import Common.View.Page as Page
import Html exposing (Html, a, div, h5, p, small, text)
import Html.Attributes exposing (class, href)
import Http
import Routing


type alias Model =
    { packages : ActionResult (List Package) }


type Msg
    = GetPackagesCompleted (Result Http.Error (List Package))


initEmpty : Model
initEmpty =
    { packages = Loading }


init : AppState -> ( Model, Cmd Msg )
init appState =
    ( initEmpty
    , Requests.getPackages appState GetPackagesCompleted
    )


update : Msg -> Model -> Model
update msg model =
    case msg of
        GetPackagesCompleted result ->
            case result of
                Ok packages ->
                    { model | packages = Success packages }

                Err _ ->
                    { model | packages = Error "Unable to get packages." }


view : Model -> Html Msg
view model =
    Page.actionResultView viewList model.packages


viewList : List Package -> Html Msg
viewList packages =
    div []
        [ div [ class "list-group list-group-flush" ]
            (List.map viewItem <| List.sortBy .name packages)
        ]


viewItem : Package -> Html Msg
viewItem package =
    let
        packageLink =
            Routing.toString <| Routing.KMDetail package.id
    in
    div [ class "list-group-item flex-column align-items-start" ]
        [ div [ class "d-flex justify-content-between" ]
            [ h5 [ class "mb-1" ]
                [ a [ href packageLink ] [ text package.name ]
                ]
            , small [] [ text package.organization.name ]
            ]
        , p [] [ text package.description ]
        ]
