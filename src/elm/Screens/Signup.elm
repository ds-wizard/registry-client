module Screens.Signup exposing
    ( Model
    , Msg(..)
    , init
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Common.AppState exposing (AppState)
import Common.FormView as FormView
import Common.Requests as Requests
import Form exposing (Form)
import Form.Validate as Validate exposing (Validation)
import Html exposing (Html, div, h1, i, p, text)
import Html.Attributes exposing (class)
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
    div []
        [ div [ class "ui icon positive message" ]
            [ i [ class "check circle outline icon" ] []
            , div [ class "content" ]
                [ div [ class "header" ] [ text "Sign up successful" ]
                , p [] [ text "Check your email address for the activation link." ]
                ]
            ]
        ]


formView : Model -> Html Msg
formView model =
    div []
        [ h1 [ class "ui header" ] [ text "Sign up" ]
        , FormView.form
            { loading = ActionResult.isLoading model.signingUp
            , error = ActionResult.isError model.signingUp
            , submitMsg = FormMsg Form.Submit
            }
            [ div [ class "ui error message" ]
                [ div [ class "header" ] [ text "Sign up failed" ]
                , p [] [ text "Something went wrong while submitting the form." ]
                ]
            , FormView.input { label = "Organization ID", field = "organizationId", tagger = FormMsg } model.form
            , FormView.input { label = "Organization Name", field = "name", tagger = FormMsg } model.form
            , FormView.input { label = "Email", field = "email", tagger = FormMsg } model.form
            , FormView.textarea { label = "Organization Description", field = "description", tagger = FormMsg } model.form
            , FormView.submit { label = "Sign Up" }
            ]
        ]
