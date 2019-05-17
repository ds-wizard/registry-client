module Main exposing (main)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Common.AppState as AppState exposing (AppState)
import Html exposing (Html, a, div, h4, i, img, li, p, text, ul)
import Html.Attributes exposing (class, href, src)
import Json.Decode as D
import Routing
import Screens.Index as Index
import Screens.KMDetail as KMDetail
import Screens.Login as Login
import Screens.OrganizationDetail as OrganizationDetail
import Screens.Signup as Signup
import Screens.SignupConfirmation as SignupConfirmation
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
    , signupConfirmationModel : SignupConfirmation.Model
    , loginModel : Login.Model
    , organizationDetailModel : OrganizationDetail.Model
    }


type Msg
    = UrlChanged Url.Url
    | LinkedClicked UrlRequest
    | IndexMsg Index.Msg
    | KMDetailMsg KMDetail.Msg
    | SignupMsg Signup.Msg
    | ConfirmSignupMsg SignupConfirmation.Msg
    | LoginMsg Login.Msg
    | OrganizationDetailMsg OrganizationDetail.Msg


init : D.Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    initChildModel
        { route = Routing.toRoute url
        , key = key
        , appState = AppState.init flags
        , indexModel = Index.initEmpty
        , kmDetailModel = KMDetail.initEmpty
        , signupModel = Signup.init
        , signupConfirmationModel = SignupConfirmation.initEmpty
        , loginModel = Login.init
        , organizationDetailModel = OrganizationDetail.init
        }


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
            initChildModel { model | route = Routing.toRoute url }

        IndexMsg indexMsg ->
            ( { model | indexModel = Index.update indexMsg model.indexModel }
            , Cmd.none
            )

        KMDetailMsg kmDetailMsg ->
            ( { model | kmDetailModel = KMDetail.update kmDetailMsg model.kmDetailModel }
            , Cmd.none
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
            ( { model | signupConfirmationModel = SignupConfirmation.update confirmSignupMsg model.signupConfirmationModel }
            , Cmd.none
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


initChildModel : Model -> ( Model, Cmd Msg )
initChildModel model =
    case model.route of
        Routing.Index ->
            let
                ( newIndexModel, indexCmd ) =
                    Index.init model.appState
            in
            ( { model | indexModel = newIndexModel }
            , Cmd.map IndexMsg indexCmd
            )

        Routing.KMDetail pkgId ->
            let
                ( newKmDetailModel, kmDetailCmd ) =
                    KMDetail.init model.appState pkgId
            in
            ( { model | kmDetailModel = newKmDetailModel }
            , Cmd.map KMDetailMsg kmDetailCmd
            )

        Routing.ConfirmSignup organizationId hash ->
            let
                ( newSignupConfirmationModel, signupConfirmationCmd ) =
                    SignupConfirmation.init model.appState organizationId hash
            in
            ( { model | signupConfirmationModel = newSignupConfirmationModel }
            , Cmd.map ConfirmSignupMsg signupConfirmationCmd
            )

        _ ->
            ( model, Cmd.none )


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

                    Routing.KMDetail _ ->
                        Html.map KMDetailMsg <| KMDetail.view model.kmDetailModel

                    Routing.Signup ->
                        Html.map SignupMsg <| Signup.view model.signupModel

                    Routing.ConfirmSignup _ _ ->
                        Html.map ConfirmSignupMsg <| SignupConfirmation.view model.signupConfirmationModel

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
