module Pages.ForgottenTokenConfirmation exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Common.AppState exposing (AppState)
import Common.Entities.ApiError as ApiError exposing (ApiError)
import Common.Entities.OrganizationDetail exposing (OrganizationDetail)
import Common.Requests as Requests
import Common.View.Page as Page
import Html exposing (Html, div, h1, p, strong, text)
import Html.Attributes exposing (class)


init : AppState -> String -> String -> ( Model, Cmd Msg )
init appState organizationId hash =
    ( { organization = Loading }
    , Requests.putOrganizationToken
        { organizationId = organizationId
        , hash = hash
        }
        appState
        PutOrganizationTokenCompleted
    )



-- MODEL


type alias Model =
    { organization : ActionResult OrganizationDetail }


setOrganization : ActionResult OrganizationDetail -> Model -> Model
setOrganization organization model =
    { model | organization = organization }



-- UPDATE


type Msg
    = PutOrganizationTokenCompleted (Result ApiError OrganizationDetail)


update : Msg -> Model -> Model
update msg =
    case msg of
        PutOrganizationTokenCompleted result ->
            ActionResult.apply setOrganization (ApiError.toActionResult "Unable to recover your organization token.") result



-- VIEW


view : Model -> Html Msg
view model =
    Page.actionResultView viewOrganization model.organization


viewOrganization : OrganizationDetail -> Html Msg
viewOrganization organization =
    div []
        [ h1 [] [ text "Recovered" ]
        , p []
            [ text "A new token for your organization "
            , strong [] [ text organization.name ]
            , text " has been generated!"
            ]
        , div [ class "alert alert-info" ]
            [ text "You will use the following token for authentication. Save it to a safe place. You will not be able to see it again." ]
        , div [ class "card" ]
            [ div [ class "card-header" ] [ text "Token" ]
            , div [ class "card-body" ] [ text organization.token ]
            ]
        ]
