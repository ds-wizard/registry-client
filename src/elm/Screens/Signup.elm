module Screens.Signup exposing
    ( Model
    , Msg(..)
    , init
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Common.AppState exposing (AppState)
import Common.Requests as Requests
import Common.View.ActionButton as ActionButton
import Common.View.FormGroup as FormGroup
import Form exposing (Form)
import Form.Validate as Validate exposing (Validation)
import Html exposing (Html, div, form, h1, h4, p, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onSubmit)
import Http
import Result exposing (Result)
import Utils exposing (validateRegex)


type alias Model =
    { form : Form () SignupForm
    , signingUp : ActionResult ()
    }


type alias SignupForm =
    { organizationId : String
    , name : String
    , email : String
    , description : String
    }


signupFormValidation : Validation e SignupForm
signupFormValidation =
    Validate.map4 SignupForm
        (Validate.field "organizationId" (validateRegex "^^(?![.])(?!.*[.]$)[a-zA-Z0-9.]+$"))
        (Validate.field "name" Validate.string)
        (Validate.field "email" Validate.email)
        (Validate.field "description" Validate.string)


initSignupForm : Form e SignupForm
initSignupForm =
    Form.initial [] signupFormValidation


type Msg
    = FormMsg Form.Msg
    | PostOrganizationCompleted (Result Http.Error ())


init : Model
init =
    { form = initSignupForm
    , signingUp = Unset
    }


update : Msg -> AppState -> Model -> ( Model, Cmd Msg )
update msg appState model =
    case msg of
        FormMsg formMsg ->
            handleFormMsg formMsg appState model

        PostOrganizationCompleted result ->
            ( { model
                | signingUp =
                    case result of
                        Ok _ ->
                            Success ()

                        Err _ ->
                            Error ""
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


view : Model -> Html Msg
view model =
    if ActionResult.isSuccess model.signingUp then
        successView

    else
        formView model


successView : Html Msg
successView =
    div [ class "alert alert-success" ]
        [ h4 [ class "alert-heading" ] [ text "Sign up successful" ]
        , p [] [ text "Check your email address for the activation link." ]
        ]


formView : Model -> Html Msg
formView model =
    let
        error =
            if ActionResult.isError model.signingUp then
                div [ class "alert alert-danger" ]
                    [ text "Something went wrong while submitting the form." ]

            else
                text ""
    in
    div []
        [ h1 [] [ text "Sign up" ]
        , form [ onSubmit <| FormMsg Form.Submit ]
            [ error
            , Html.map FormMsg <| FormGroup.input model.form "organizationId" "Organization ID"
            , Html.map FormMsg <| FormGroup.input model.form "name" "Organization Name"
            , Html.map FormMsg <| FormGroup.input model.form "email" "Email"
            , Html.map FormMsg <| FormGroup.textarea model.form "description" "Organization Description"
            , ActionButton.submit ( "Sign up", model.signingUp )
            ]
        ]
