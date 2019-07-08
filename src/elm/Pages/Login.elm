module Pages.Login exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Common.AppState as AppState exposing (AppState)
import Common.Entities.ApiError exposing (ApiError)
import Common.FormExtra exposing (CustomFormError)
import Common.Requests as Requests
import Common.View.ActionButton as ActionButton
import Common.View.FormGroup as FormGroup
import Common.View.FormResult as FormResult
import Form exposing (Form)
import Form.Validate as Validate exposing (Validation)
import Html exposing (Html, a, div, form, text)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onSubmit)
import Routing


init : Model
init =
    { form = initLoginForm
    , loggingIn = Unset
    }



-- MODEL


type alias Model =
    { form : Form CustomFormError LoginForm
    , loggingIn : ActionResult ()
    }


type alias LoginForm =
    { organizationId : String
    , token : String
    }


loginFormValidation : Validation e LoginForm
loginFormValidation =
    Validate.map2 LoginForm
        (Validate.field "organizationId" Validate.string)
        (Validate.field "token" Validate.string)


initLoginForm : Form e LoginForm
initLoginForm =
    Form.initial [] loginFormValidation



-- UPDATE


type Msg
    = FormMsg Form.Msg
    | GetTokenCompleted (Result ApiError String)


update :
    { tagger : Msg -> msg
    , loginCmd : AppState.Credentials -> Cmd msg
    }
    -> Msg
    -> AppState
    -> Model
    -> ( Model, Cmd msg )
update { tagger, loginCmd } msg appState model =
    case msg of
        FormMsg formMsg ->
            handleFormMsg tagger formMsg appState model

        GetTokenCompleted result ->
            case ( result, Form.getOutput model.form ) of
                ( Ok _, Just loginForm ) ->
                    ( model, loginCmd loginForm )

                _ ->
                    ( { model | loggingIn = Error "Invalid organization ID and token combination." }
                    , Cmd.none
                    )


handleFormMsg : (Msg -> msg) -> Form.Msg -> AppState -> Model -> ( Model, Cmd msg )
handleFormMsg tagger formMsg appState model =
    case ( formMsg, Form.getOutput model.form ) of
        ( Form.Submit, Just loginForm ) ->
            ( { model | loggingIn = Loading }
            , Requests.getToken loginForm appState GetTokenCompleted
                |> Cmd.map tagger
            )

        _ ->
            ( { model | form = Form.update loginFormValidation formMsg model.form }
            , Cmd.none
            )



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "card card-form bg-light" ]
        [ div [ class "card-header" ] [ text "Log in" ]
        , div [ class "card-body" ]
            [ form [ onSubmit <| FormMsg Form.Submit ]
                [ FormResult.errorOnlyView model.loggingIn
                , Html.map FormMsg <| FormGroup.input model.form "organizationId" "Organization ID"
                , Html.map FormMsg <| FormGroup.password model.form "token" "Token"
                , div [ class "d-flex justify-content-between align-items-center" ]
                    [ ActionButton.submit ( "Log in", model.loggingIn )
                    , a [ href <| Routing.toString Routing.ForgottenToken ] [ text "Forgot your token?" ]
                    ]
                ]
            ]
        ]
