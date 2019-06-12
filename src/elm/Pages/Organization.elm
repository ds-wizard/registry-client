module Pages.Organization exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Common.AppState as AppState exposing (AppState)
import Common.Entities.OrganizationDetail exposing (OrganizationDetail)
import Common.Requests as Requests
import Common.View.ActionButton as ActionButton
import Common.View.FormGroup as FormGroup
import Common.View.FormResult as FormResult
import Form exposing (Form)
import Form.Field as Field exposing (Field)
import Form.Validate as Validate exposing (Validation)
import Html exposing (Html, div, form, h1, text)
import Html.Events exposing (onSubmit)
import Http


type alias Model =
    { organization : ActionResult OrganizationDetail
    , form : Form () OrganizationForm
    , saving : ActionResult String
    }


type alias OrganizationForm =
    { name : String
    , description : String
    , email : String
    }


setOrganization : ActionResult OrganizationDetail -> Model -> Model
setOrganization organization model =
    let
        form =
            case organization of
                Success data ->
                    initOrganizationForm <| organizationFormInitials data

                _ ->
                    model.form
    in
    { model
        | organization = organization
        , form = form
    }


organizationFormValidation : Validation e OrganizationForm
organizationFormValidation =
    Validate.map3 OrganizationForm
        (Validate.field "name" Validate.string)
        (Validate.field "description" Validate.string)
        (Validate.field "email" Validate.email)


organizationFormInitials : OrganizationDetail -> List ( String, Field )
organizationFormInitials organization =
    [ ( "name", Field.string organization.name )
    , ( "description", Field.string organization.description )
    , ( "email", Field.string organization.email )
    ]


initOrganizationForm : List ( String, Field ) -> Form e OrganizationForm
initOrganizationForm initials =
    Form.initial initials organizationFormValidation


init : AppState -> AppState.Credentials -> ( Model, Cmd Msg )
init appState credentials =
    ( { organization = Loading
      , form = initOrganizationForm []
      , saving = Unset
      }
    , Requests.getOrganization credentials appState GetOrganizationCompleted
    )


type Msg
    = FormMsg Form.Msg
    | GetOrganizationCompleted (Result Http.Error OrganizationDetail)
    | PutOrganizationCompleted (Result Http.Error OrganizationDetail)


update : Msg -> AppState -> Model -> ( Model, Cmd Msg )
update msg appState model =
    case msg of
        FormMsg formMsg ->
            handleFormMsg formMsg appState model

        GetOrganizationCompleted result ->
            ( ActionResult.apply setOrganization "Unable to get organization detail." result model
            , Cmd.none
            )

        PutOrganizationCompleted result ->
            let
                newModel =
                    case result of
                        Ok organization ->
                            { model
                                | saving = Success "Your changes has been saved."
                                , form = initOrganizationForm <| organizationFormInitials organization
                            }

                        Err _ ->
                            { model | saving = Error "Unable to save changes." }
            in
            ( newModel, Cmd.none )


handleFormMsg : Form.Msg -> AppState -> Model -> ( Model, Cmd Msg )
handleFormMsg formMsg appState model =
    case ( formMsg, Form.getOutput model.form ) of
        ( Form.Submit, Just form ) ->
            ( { model | saving = Loading }
            , Requests.putOrganization form appState PutOrganizationCompleted
            )

        _ ->
            ( { model | form = Form.update organizationFormValidation formMsg model.form }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Edit organization" ]
        , form [ onSubmit <| FormMsg Form.Submit ]
            [ FormResult.view model.saving
            , Html.map FormMsg <| FormGroup.input model.form "name" "Organization Name"
            , Html.map FormMsg <| FormGroup.textarea model.form "description" "Organization Description"
            , Html.map FormMsg <| FormGroup.input model.form "email" "Email"
            , ActionButton.submit ( "Save", model.saving )
            ]
        ]
