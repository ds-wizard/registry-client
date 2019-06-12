module Main exposing (main)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Common.AppState as AppState exposing (AppState)
import Common.View.Page as Page
import Html exposing (Html, a, div, img, li, text, ul)
import Html.Attributes exposing (class, href, src)
import Html.Events exposing (onClick)
import Json.Decode as D
import Json.Encode as E
import Pages.ForgottenToken as ForgottenToken
import Pages.ForgottenTokenConfirmation as ForgottenTokenConfirmation
import Pages.Index as Index
import Pages.KMDetail as KMDetail
import Pages.Login as Login
import Pages.Organization as Organization
import Pages.Signup as Signup
import Pages.SignupConfirmation as SignupConfirmation
import Ports
import Routing
import Url
import Utils exposing (dispatch)


main : Program D.Value Model Msg
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
    , pageModel : PageModel
    }


type PageModel
    = ForgottenTokenModel ForgottenToken.Model
    | ForgottenTokenConfirmationModel ForgottenTokenConfirmation.Model
    | IndexModel Index.Model
    | KMDetailModel KMDetail.Model
    | LoginModel Login.Model
    | OrganizationModel Organization.Model
    | SignupModel Signup.Model
    | SignupConfirmationModel SignupConfirmation.Model
    | NotFoundModel


type Msg
    = UrlChanged Url.Url
    | LinkedClicked UrlRequest
    | SetCredentials (Maybe AppState.Credentials)
    | ForgottenTokenMsg ForgottenToken.Msg
    | ForgottenTokenConfirmationMsg ForgottenTokenConfirmation.Msg
    | IndexMsg Index.Msg
    | KMDetailMsg KMDetail.Msg
    | LoginMsg Login.Msg
    | OrganizationMsg Organization.Msg
    | SignupMsg Signup.Msg
    | SignupConfirmationMsg SignupConfirmation.Msg


init : D.Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    initChildModel
        { route = Routing.toRoute url
        , key = key
        , appState = AppState.init flags
        , pageModel = NotFoundModel
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.pageModel ) of
        ( LinkedClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        ( UrlChanged url, _ ) ->
            initChildModel { model | route = Routing.toRoute url }

        ( SetCredentials mbCredentials, _ ) ->
            let
                ( route, encodedCredentials ) =
                    case mbCredentials of
                        Just credentials ->
                            ( Routing.Organization
                            , AppState.encodeCredentials credentials
                            )

                        Nothing ->
                            ( Routing.Index
                            , E.null
                            )
            in
            ( { model | appState = AppState.setCredentials mbCredentials model.appState }
            , Cmd.batch
                [ Nav.pushUrl model.key <| Routing.toString route
                , Ports.saveCredentials encodedCredentials
                ]
            )

        ( ForgottenTokenMsg forgottenTokenMsg, ForgottenTokenModel forgottenTokenModel ) ->
            let
                ( newForgottenTokenModel, forgottenTokenCmd ) =
                    ForgottenToken.update forgottenTokenMsg model.appState forgottenTokenModel
            in
            ( { model | pageModel = ForgottenTokenModel newForgottenTokenModel }
            , Cmd.map ForgottenTokenMsg forgottenTokenCmd
            )

        ( ForgottenTokenConfirmationMsg forgottenTokenConfirmationMsg, ForgottenTokenConfirmationModel forgottenTokenConfirmationModel ) ->
            ( { model | pageModel = ForgottenTokenConfirmationModel <| ForgottenTokenConfirmation.update forgottenTokenConfirmationMsg forgottenTokenConfirmationModel }
            , Cmd.none
            )

        ( IndexMsg indexMsg, IndexModel indexModel ) ->
            ( { model | pageModel = IndexModel <| Index.update indexMsg indexModel }
            , Cmd.none
            )

        ( KMDetailMsg kmDetailMsg, KMDetailModel kmDetailModel ) ->
            ( { model | pageModel = KMDetailModel <| KMDetail.update kmDetailMsg kmDetailModel }
            , Cmd.none
            )

        ( LoginMsg loginMsg, LoginModel loginModel ) ->
            let
                ( newLoginModel, cmd ) =
                    Login.update
                        { tagger = LoginMsg
                        , loginCmd = \c -> dispatch <| SetCredentials <| Just c
                        }
                        loginMsg
                        model.appState
                        loginModel
            in
            ( { model | pageModel = LoginModel <| newLoginModel }
            , cmd
            )

        ( OrganizationMsg organizationMsg, OrganizationModel organizationModel ) ->
            let
                ( newOrganizationModel, cmd ) =
                    Organization.update organizationMsg model.appState organizationModel
            in
            ( { model | pageModel = OrganizationModel newOrganizationModel }
            , Cmd.map OrganizationMsg cmd
            )

        ( SignupMsg signupMsg, SignupModel signupModel ) ->
            let
                ( newSignupModel, cmd ) =
                    Signup.update signupMsg model.appState signupModel
            in
            ( { model | pageModel = SignupModel newSignupModel }
            , Cmd.map SignupMsg cmd
            )

        ( SignupConfirmationMsg confirmSignupMsg, SignupConfirmationModel signupConfirmationModel ) ->
            ( { model | pageModel = SignupConfirmationModel <| SignupConfirmation.update confirmSignupMsg signupConfirmationModel }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


initChildModel : Model -> ( Model, Cmd Msg )
initChildModel model =
    case model.route of
        Routing.ForgottenToken ->
            withOrganizationRedirect
                ( { model | pageModel = ForgottenTokenModel ForgottenToken.init }
                , Cmd.none
                )

        Routing.ForgottenTokenConfirmation organizationId hash ->
            let
                ( forgottenTokenConfirmationModel, forgottenTokenConfirmationCmd ) =
                    ForgottenTokenConfirmation.init model.appState organizationId hash
            in
            withOrganizationRedirect
                ( { model | pageModel = ForgottenTokenConfirmationModel forgottenTokenConfirmationModel }
                , Cmd.map ForgottenTokenConfirmationMsg forgottenTokenConfirmationCmd
                )

        Routing.Index ->
            let
                ( indexModel, indexCmd ) =
                    Index.init model.appState
            in
            ( { model | pageModel = IndexModel indexModel }
            , Cmd.map IndexMsg indexCmd
            )

        Routing.KMDetail pkgId ->
            let
                ( kmDetailModel, kmDetailCmd ) =
                    KMDetail.init model.appState pkgId
            in
            ( { model | pageModel = KMDetailModel kmDetailModel }
            , Cmd.map KMDetailMsg kmDetailCmd
            )

        Routing.Login ->
            withOrganizationRedirect
                ( { model | pageModel = LoginModel Login.init }
                , Cmd.none
                )

        Routing.Organization ->
            case model.appState.credentials of
                Just credentials ->
                    let
                        ( organizationModel, organizationCmd ) =
                            Organization.init model.appState credentials
                    in
                    ( { model | pageModel = OrganizationModel organizationModel }
                    , Cmd.map OrganizationMsg organizationCmd
                    )

                Nothing ->
                    ( model
                    , Nav.pushUrl model.key <| Routing.toString Routing.Login
                    )

        Routing.Signup ->
            withOrganizationRedirect
                ( { model | pageModel = SignupModel Signup.init }
                , Cmd.none
                )

        Routing.SignupConfirmation organizationId hash ->
            let
                ( signupConfirmationModel, signupConfirmationCmd ) =
                    SignupConfirmation.init model.appState organizationId hash
            in
            withOrganizationRedirect
                ( { model | pageModel = SignupConfirmationModel signupConfirmationModel }
                , Cmd.map SignupConfirmationMsg signupConfirmationCmd
                )

        Routing.NotFound ->
            ( { model | pageModel = NotFoundModel }
            , Cmd.none
            )


withOrganizationRedirect : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
withOrganizationRedirect ( model, cmd ) =
    case model.appState.credentials of
        Just _ ->
            ( model, Nav.pushUrl model.key <| Routing.toString Routing.Organization )

        Nothing ->
            ( model, cmd )


view : Model -> Document Msg
view model =
    let
        content =
            if not model.appState.valid then
                Page.illustratedMessage
                    { image = "bug_fixing"
                    , heading = "Configuration Error"
                    , msg = "Application is not configured correctly and cannot run."
                    }

            else
                case model.pageModel of
                    ForgottenTokenModel forgottenTokenModel ->
                        Html.map ForgottenTokenMsg <| ForgottenToken.view forgottenTokenModel

                    ForgottenTokenConfirmationModel forgottenTokenConfirmationModel ->
                        Html.map ForgottenTokenConfirmationMsg <| ForgottenTokenConfirmation.view forgottenTokenConfirmationModel

                    IndexModel indexModel ->
                        Html.map IndexMsg <| Index.view indexModel

                    KMDetailModel kmDetailModel ->
                        Html.map KMDetailMsg <| KMDetail.view kmDetailModel

                    LoginModel loginModel ->
                        Html.map LoginMsg <| Login.view loginModel

                    OrganizationModel organizationDetailModel ->
                        Html.map OrganizationMsg <| Organization.view organizationDetailModel

                    SignupModel signupModel ->
                        Html.map SignupMsg <| Signup.view signupModel

                    SignupConfirmationModel signupConfirmationModel ->
                        Html.map SignupConfirmationMsg <| SignupConfirmation.view signupConfirmationModel

                    NotFoundModel ->
                        Page.illustratedMessage
                            { image = "page_not_found"
                            , heading = "Not Found"
                            , msg = "The page you are looking for does not exist."
                            }

        html =
            [ header model.appState
            , div [ class "container" ]
                [ content ]
            ]
    in
    { title = "Registry"
    , body = html
    }


header : AppState -> Html Msg
header appState =
    let
        navigation =
            appState.credentials
                |> Maybe.map (always loggedInHeaderNavigation)
                |> Maybe.withDefault publicHeaderNavigation
    in
    div [ class "navbar navbar-expand-lg fixed-top navbar-light bg-light" ]
        [ div [ class "container" ]
            [ a [ class "navbar-brand", href <| Routing.toString Routing.Index ]
                [ img [ class "logo", src "/img/logo.svg" ] []
                , text "Registry"
                ]
            , navigation
            ]
        ]


loggedInHeaderNavigation : Html Msg
loggedInHeaderNavigation =
    div []
        [ ul [ class "nav navbar-nav ml-auto" ]
            [ li [ class "nav-item" ]
                [ a
                    [ href <| Routing.toString Routing.Organization
                    , class "nav-link"
                    ]
                    [ text "Profile" ]
                ]
            , li [ class "nav-item" ]
                [ a
                    [ onClick <| SetCredentials Nothing
                    , class "nav-link"
                    ]
                    [ text "Log out" ]
                ]
            ]
        ]


publicHeaderNavigation : Html Msg
publicHeaderNavigation =
    div []
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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
