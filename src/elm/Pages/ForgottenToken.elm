module Pages.ForgottenToken exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Common.AppState as AppState exposing (AppState)
import Common.Requests as Requests
import Common.View.ActionButton as ActionButton
import Common.View.FormGroup as FormGroup
import Common.View.FormResult as FormResult
import Common.View.Page as Page
import Form exposing (Form)
import Form.Validate as Validate exposing (Validation)
import Html exposing (Html, div, form, h1, p, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onSubmit)
import Http


type alias Model =
    { form : Form () RecoveryForm
    , submitting : ActionResult ()
    }


type alias RecoveryForm =
    { email : String }


recoveryFormValidation : Validation e RecoveryForm
recoveryFormValidation =
    Validate.map RecoveryForm
        (Validate.field "email" Validate.email)


initRecoveryForm : Form e RecoveryForm
initRecoveryForm =
    Form.initial [] recoveryFormValidation


type Msg
    = FormMsg Form.Msg
    | PostRegistrationActionKeyCompleted (Result Http.Error ())


init : Model
init =
    { form = initRecoveryForm
    , submitting = Unset
    }


update : Msg -> AppState -> Model -> ( Model, Cmd Msg )
update msg appState model =
    case msg of
        FormMsg formMsg ->
            handleFormMsg formMsg appState model

        PostRegistrationActionKeyCompleted result ->
            let
                submitting =
                    case result of
                        Ok _ ->
                            Success ()

                        Err _ ->
                            Error "Could not recover token."
            in
            ( { model | submitting = submitting }
            , Cmd.none
            )


handleFormMsg : Form.Msg -> AppState -> Model -> ( Model, Cmd Msg )
handleFormMsg formMsg appState model =
    case ( formMsg, Form.getOutput model.form ) of
        ( Form.Submit, Just recoveryForm ) ->
            ( { model | submitting = Loading }
            , Requests.postForgottenTokenActionKey recoveryForm appState PostRegistrationActionKeyCompleted
            )

        _ ->
            ( { model | form = Form.update recoveryFormValidation formMsg model.form }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    if ActionResult.isSuccess model.submitting then
        successView

    else
        formView model


successView : Html Msg
successView =
    Page.illustratedMessage
        { image = "confirmation"
        , heading = "Token recovery successful!"
        , msg = "Check your email address for the recovery link."
        }


formView : Model -> Html Msg
formView model =
    div []
        [ h1 [] [ text "Forgotten Token" ]
        , form [ onSubmit <| FormMsg Form.Submit ]
            [ FormResult.errorOnlyView model.submitting
            , Html.map FormMsg <| FormGroup.input model.form "email" "Email"
            , p [ class "text-muted" ]
                [ text "Enter the email you used to register your organization." ]
            , ActionButton.submit ( "Submit", model.submitting )
            ]
        ]