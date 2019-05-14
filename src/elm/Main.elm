module Main exposing (main)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Common.AppState as AppState exposing (AppState)
import Html exposing (Html, a, div, h4, i, img, li, p, text, ul)
import Html.Attributes exposing (class, href, src)
import Json.Decode as D
import Routing
import Screens.ConfirmSignup as ConfirmSignup
import Screens.Index as Index
import Screens.KMDetail as KMDetail
import Screens.Login as Login
import Screens.OrganizationDetail as OrganizationDetail
import Screens.Signup as Signup
import Url


main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkedClicked
        }


type alias Model =
    { route : Routing.Route
    , key : Nav.Key
    , appState : AppState
    , indexModel : Index.Model
    , kmDetailModel : KMDetail.Model
    , signupModel : Signup.Model
    , confirmSignupModel : ConfirmSignup.Model
    , loginModel : Login.Model
    , organizationDetailModel : OrganizationDetail.Model
    }


type Msg
    = UrlChanged Url.Url
    | LinkedClicked UrlRequest
    | IndexMsg Index.Msg
    | KMDetailMsg KMDetail.Msg
    | SignupMsg Signup.Msg
    | ConfirmSignupMsg ConfirmSignup.Msg
    | LoginMsg Login.Msg
    | OrganizationDetailMsg OrganizationDetail.Msg


init : D.Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { route = Routing.toRoute url
      , key = key
      , appState = AppState.init flags
      , indexModel = Index.init
      , kmDetailModel = KMDetail.init
      , signupModel = Signup.init
      , confirmSignupModel = ConfirmSignup.init
      , loginModel = Login.init
      , organizationDetailModel = OrganizationDetail.init
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkedClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | route = Routing.toRoute url }
            , Cmd.none
            )

        IndexMsg indexMsg ->
            let
                ( newIndexModel, cmd ) =
                    Index.update indexMsg model.indexModel
            in
            ( { model | indexModel = newIndexModel }
            , Cmd.map IndexMsg cmd
            )

        KMDetailMsg kmDetailMsg ->
            let
                ( newKmDetailModel, cmd ) =
                    KMDetail.update kmDetailMsg model.kmDetailModel
            in
            ( { model | kmDetailModel = newKmDetailModel }
            , Cmd.map KMDetailMsg cmd
            )

        SignupMsg signupMsg ->
            let
                ( newSignupModel, cmd ) =
                    Signup.update signupMsg model.appState model.signupModel
            in
            ( { model | signupModel = newSignupModel }
            , Cmd.map SignupMsg cmd
            )

        ConfirmSignupMsg confirmSignupMsg ->
            let
                ( newConfirmSignupModel, cmd ) =
                    ConfirmSignup.update confirmSignupMsg model.confirmSignupModel
            in
            ( { model | confirmSignupModel = newConfirmSignupModel }
            , Cmd.map ConfirmSignupMsg cmd
            )

        LoginMsg loginMsg ->
            let
                ( newLoginModel, cmd ) =
                    Login.update loginMsg model.loginModel
            in
            ( { model | loginModel = newLoginModel }
            , Cmd.map LoginMsg cmd
            )

        OrganizationDetailMsg organizationDetailMsg ->
            let
                ( newOrganizationDetailModel, cmd ) =
                    OrganizationDetail.update organizationDetailMsg model.organizationDetailModel
            in
            ( { model | organizationDetailModel = newOrganizationDetailModel }
            , Cmd.map OrganizationDetailMsg cmd
            )


view : Model -> Document Msg
view model =
    let
        content =
            if not model.appState.valid then
                misconfigured

            else
                case model.route of
                    Routing.Index ->
                        Html.map IndexMsg <| Index.view model.indexModel

                    Routing.KMDetail _ _ _ ->
                        Html.map KMDetailMsg <| KMDetail.view model.kmDetailModel

                    Routing.Signup ->
                        Html.map SignupMsg <| Signup.view model.signupModel

                    Routing.ConfirmSignup _ _ ->
                        Html.map ConfirmSignupMsg <| ConfirmSignup.view model.confirmSignupModel

                    Routing.Login ->
                        Html.map LoginMsg <| Login.view model.loginModel

                    Routing.OrganizationDetail _ ->
                        Html.map OrganizationDetailMsg <| OrganizationDetail.view model.organizationDetailModel

                    Routing.NotFound ->
                        div [] [ text "Not found" ]

        html =
            [ header
            , div [ class "container" ]
                [ content ]
            ]
    in
    { title = "Registry"
    , body = html
    }


header : Html Msg
header =
    div [ class "navbar navbar-expand-lg fixed-top navbar-light bg-light" ]
        [ div [ class "container" ]
            [ a [ class "navbar-brand", href <| Routing.toString Routing.Index ]
                [ img [ class "logo", src "/img/logo.svg" ] []
                , text "Registry"
                ]
            , div []
                [ ul [ class "nav navbar-nav ml-auto" ]
                    [ li [ class "nav-item" ]
                        [ a [ href <| Routing.toString Routing.Login, class "nav-link" ]
                            [ text "Log in" ]
                        ]
                    , li [ class "nav-item" ]
                        [ a [ href <| Routing.toString Routing.Signup, class "nav-link" ]
                            [ text "Sign up" ]
                        ]
                    ]
                ]
            ]
        ]


misconfigured : Html msg
misconfigured =
    div [ class "alert alert-danger" ]
        [ h4 [ class "alert-heading" ] [ text "Configuration Error" ]
        , p [] [ text "Application is not configured correctly and cannot run." ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
