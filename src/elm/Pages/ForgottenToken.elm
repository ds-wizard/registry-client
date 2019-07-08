module Pages.ForgottenToken exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Common.AppState as AppState exposing (AppState)
import Common.Entities.ApiError as ApiError exposing (ApiError)
import Common.FormExtra exposing (CustomFormError)
import Common.Requests as Requests
import Common.View.ActionButton as ActionButton
import Common.View.FormGroup as FormGroup
import Common.View.FormResult as FormResult
import Common.View.Page as Page
import Form exposing (Form)
import Form.Validate as Validate exposing (Validation)
import Html exposing (Html, div, form, p, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onSubmit)
import Result exposing (Result)


init : Model
init =
    { form = initRecoveryForm
    , submitting = Unset
    }



-- MODEL


type alias Model =
    { form : Form CustomFormError RecoveryForm
    , submitting : ActionResult ()
    }


type alias RecoveryForm =
    { email : String }


setSubmitting : ActionResult () -> Model -> Model
setSubmitting submitting model =
    { model | submitting = submitting }


recoveryFormValidation : Validation e RecoveryForm
recoveryFormValidation =
    Validate.map RecoveryForm
        (Validate.field "email" Validate.email)


initRecoveryForm : Form e RecoveryForm
initRecoveryForm =
    Form.initial [] recoveryFormValidation



-- UPDATE


type Msg
    = FormMsg Form.Msg
    | PostForgottenTokenActionKeyCompleted (Result ApiError ())


update : Msg -> AppState -> Model -> ( Model, Cmd Msg )
update msg appState model =
    case msg of
        FormMsg formMsg ->
            handleFormMsg formMsg appState model

        PostForgottenTokenActionKeyCompleted result ->
            ( ActionResult.apply setSubmitting (ApiError.toActionResult "Could not recover token.") result model
            , Cmd.none
            )


handleFormMsg : Form.Msg -> AppState -> Model -> ( Model, Cmd Msg )
handleFormMsg formMsg appState model =
    case ( formMsg, Form.getOutput model.form ) of
        ( Form.Submit, Just recoveryForm ) ->
            ( { model | submitting = Loading }
            , Requests.postForgottenTokenActionKey recoveryForm appState PostForgottenTokenActionKeyCompleted
            )

        _ ->
            ( { model | form = Form.update recoveryFormValidation formMsg model.form }
            , Cmd.none
            )



-- VIEW


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
    div [ class "card card-form bg-light" ]
        [ div [ class "card-header" ] [ text "Forgotten Token" ]
        , div [ class "card-body" ]
            [ form [ onSubmit <| FormMsg Form.Submit ]
                [ FormResult.errorOnlyView model.submitting
                , Html.map FormMsg <| FormGroup.input model.form "email" "Email"
                , p [ class "text-muted" ]
                    [ text "Enter the email you used to register your organization." ]
                , ActionButton.submit ( "Submit", model.submitting )
                ]
            ]
        ]
