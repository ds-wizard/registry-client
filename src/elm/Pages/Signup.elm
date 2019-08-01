module Pages.Signup exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Common.AppState exposing (AppState)
import Common.Entities.ApiError as ApiError exposing (ApiError)
import Common.FormExtra exposing (CustomFormError, setFormErrors)
import Common.Requests as Requests
import Common.View.ActionButton as ActionButton
import Common.View.FormGroup as FormGroup
import Common.View.FormResult as FormResult
import Common.View.Page as Page
import Form exposing (Form)
import Form.Error as Error exposing (Error, ErrorValue(..))
import Form.Field as Field exposing (Field)
import Form.Input as Input
import Form.Validate as Validate exposing (Validation)
import Html exposing (Html, a, div, form, label, p, text)
import Html.Attributes exposing (class, classList, for, href, id, name, target)
import Html.Events exposing (onSubmit)
import Result exposing (Result)
import Utils exposing (validateRegex)


init : Model
init =
    { form = initSignupForm
    , signingUp = Unset
    }



-- MODEL


type alias Model =
    { form : Form CustomFormError SignupForm
    , signingUp : ActionResult ()
    }


type alias SignupForm =
    { organizationId : String
    , name : String
    , email : String
    , description : String
    , accept : Bool
    }


signupFormValidation : Validation e SignupForm
signupFormValidation =
    Validate.map5 SignupForm
        (Validate.field "organizationId" (validateRegex "^^(?![.])(?!.*[.]$)[a-zA-Z0-9.]+$"))
        (Validate.field "name" Validate.string)
        (Validate.field "email" Validate.email)
        (Validate.field "description" Validate.string)
        (Validate.field "accept" validateAcceptField)


validateAcceptField : Field -> Result (Error customError) Bool
validateAcceptField v =
    if Field.asBool v |> Maybe.withDefault False then
        Ok True

    else
        Err (Error.value Empty)


initSignupForm : Form e SignupForm
initSignupForm =
    Form.initial [] signupFormValidation



-- UPDATE


type Msg
    = FormMsg Form.Msg
    | PostOrganizationCompleted (Result ApiError ())


update : Msg -> AppState -> Model -> ( Model, Cmd Msg )
update msg appState model =
    case msg of
        FormMsg formMsg ->
            handleFormMsg formMsg appState model

        PostOrganizationCompleted result ->
            case result of
                Ok _ ->
                    ( { model | signingUp = Success () }, Cmd.none )

                Err err ->
                    ( { model
                        | signingUp = ApiError.toActionResult "Registration was not successful." err
                        , form = setFormErrors err model.form
                      }
                    , Cmd.none
                    )


handleFormMsg : Form.Msg -> AppState -> Model -> ( Model, Cmd Msg )
handleFormMsg formMsg appState model =
    case ( formMsg, Form.getOutput model.form ) of
        ( Form.Submit, Just signupForm ) ->
            ( { model | signingUp = Loading }
            , Requests.postOrganization signupForm appState PostOrganizationCompleted
            )

        _ ->
            ( { model | form = Form.update signupFormValidation formMsg model.form }
            , Cmd.none
            )



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "Signup" ]
        [ if ActionResult.isSuccess model.signingUp then
            successView

          else
            formView model
        ]


successView : Html Msg
successView =
    Page.illustratedMessage
        { image = "confirmation"
        , heading = "Sign up successful!"
        , msg = "Check your email address for the activation link."
        }


formView : Model -> Html Msg
formView model =
    let
        acceptField =
            Form.getFieldAsBool "accept" model.form

        hasError =
            case acceptField.liveError of
                Just _ ->
                    True

                Nothing ->
                    False

        acceptGroup =
            div [ class "form-group form-group-accept", classList [ ( "has-error", hasError ) ] ]
                [ label [ for "accept" ]
                    [ Input.checkboxInput acceptField [ id "accept", name "accept" ]
                    , text "I have read "
                    , a [ href "https://ds-wizard.org/privacy.html", target "_blank" ]
                        [ text "Privacy" ]
                    , text "."
                    ]
                , p [ class "invalid-feedback" ] [ text "You have to read Privacy first" ]
                ]
    in
    div [ class "card card-form bg-light" ]
        [ div [ class "card-header" ] [ text "Sign up" ]
        , div [ class "card-body" ]
            [ form [ onSubmit <| FormMsg Form.Submit ]
                [ FormResult.errorOnlyView model.signingUp
                , Html.map FormMsg <| FormGroup.input model.form "organizationId" "Organization ID"
                , Html.map FormMsg <| FormGroup.input model.form "name" "Organization Name"
                , Html.map FormMsg <| FormGroup.input model.form "email" "Email"
                , Html.map FormMsg <| FormGroup.textarea model.form "description" "Organization Description"
                , Html.map FormMsg <| acceptGroup
                , ActionButton.submit ( "Sign up", model.signingUp )
                ]
            ]
        ]
