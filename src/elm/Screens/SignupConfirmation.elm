module Screens.SignupConfirmation exposing
    ( Model
    , Msg(..)
    , init
    , initEmpty
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Common.AppState exposing (AppState)
import Common.Entities.OrganizationDetail exposing (OrganizationDetail)
import Common.Requests as Requests
import Common.View.Page as Page
import Html exposing (Html, div, h1, p, strong, text)
import Html.Attributes exposing (class)
import Http


type alias Model =
    { organization : ActionResult OrganizationDetail }


setOrganization : ActionResult OrganizationDetail -> Model -> Model
setOrganization organization model =
    { model | organization = organization }


initEmpty : Model
initEmpty =
    { organization = Loading }


init : AppState -> String -> String -> ( Model, Cmd Msg )
init appState organizationId hash =
    ( initEmpty
    , Requests.putOrganizationState
        { organizationId = organizationId
        , hash = hash
        , active = True
        }
        appState
        PutOrganizationCompleted
    )


type Msg
    = PutOrganizationCompleted (Result Http.Error OrganizationDetail)


update : Msg -> Model -> Model
update msg =
    case msg of
        PutOrganizationCompleted result ->
            ActionResult.apply setOrganization "Unable to activate your organization account." result


view : Model -> Html Msg
view model =
    Page.actionResultView viewOrganization model.organization


viewOrganization : OrganizationDetail -> Html Msg
viewOrganization organization =
    div [ class "SignupConfirmation" ]
        [ h1 [] [ text "Activated" ]
        , p []
            [ text "The account for your organization "
            , strong [] [ text organization.name ]
            , text " has been successfully activated!"
            ]
        , div [ class "alert alert-info" ]
            [ text "You will use the following token for authentication. Save it to a safe place. You will not be able to see it again." ]
        , div [ class "card" ]
            [ div [ class "card-header" ] [ text "Token" ]
            , div [ class "card-body" ] [ text organization.token ]
            ]
        ]