module Main exposing (main)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Common.AppState as AppState exposing (AppState)
import Html exposing (Html, a, div, h4, i, img, li, p, text, ul)
import Html.Attributes exposing (class, href, src)
import Html.Events exposing (onClick)
import Json.Decode as D
import Json.Encode as E
import Pages.Index as Index
import Pages.KMDetail as KMDetail
import Pages.Login as Login
import Pages.OrganizationDetail as OrganizationDetail
import Pages.Signup as Signup
import Pages.SignupConfirmation as SignupConfirmation
import Ports
import Routing
import Url
import Utils exposing (dispatch)


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
    = IndexModel Index.Model
    | KMDetailModel KMDetail.Model
    | LoginModel Login.Model
    | OrganizationDetailModel OrganizationDetail.Model
    | SignupModel Signup.Model
    | SignupConfirmationModel SignupConfirmation.Model
    | NotFoundModel


type Msg
    = UrlChanged Url.Url
    | LinkedClicked UrlRequest
    | SetCredentials (Maybe AppState.Credentials)
    | IndexMsg Index.Msg
    | KMDetailMsg KMDetail.Msg
    | SignupMsg Signup.Msg
    | SignupConfirmationMsg SignupConfirmation.Msg
    | LoginMsg Login.Msg
    | OrganizationDetailMsg OrganizationDetail.Msg


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
                            ( Routing.OrganizationDetail credentials.organizationId
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

        ( IndexMsg indexMsg, IndexModel indexModel ) ->
            ( { model | pageModel = IndexModel <| Index.update indexMsg indexModel }
            , Cmd.none
            )

        ( KMDetailMsg kmDetailMsg, KMDetailModel kmDetailModel ) ->
            ( { model | pageModel = KMDetailModel <| KMDetail.update kmDetailMsg kmDetailModel }
            , Cmd.none
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

        ( OrganizationDetailMsg organizationDetailMsg, OrganizationDetailModel organizationDetailModel ) ->
            let
                ( newOrganizationDetailModel, cmd ) =
                    OrganizationDetail.update organizationDetailMsg organizationDetailModel
            in
            ( { model | pageModel = OrganizationDetailModel newOrganizationDetailModel }
            , Cmd.map OrganizationDetailMsg cmd
            )

        _ ->
            ( model, Cmd.none )


initChildModel : Model -> ( Model, Cmd Msg )
initChildModel model =
    case model.route of
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

        Routing.Signup ->
            ( { model | pageModel = SignupModel Signup.init }
            , Cmd.none
            )

        Routing.SignupConfirmation organizationId hash ->
            let
                ( signupConfirmationModel, signupConfirmationCmd ) =
                    SignupConfirmation.init model.appState organizationId hash
            in
            ( { model | pageModel = SignupConfirmationModel signupConfirmationModel }
            , Cmd.map SignupConfirmationMsg signupConfirmationCmd
            )

        Routing.Login ->
            ( { model | pageModel = LoginModel Login.init }
            , Cmd.none
            )

        Routing.OrganizationDetail _ ->
            ( { model | pageModel = OrganizationDetailModel OrganizationDetail.init }
            , Cmd.none
            )

        Routing.NotFound ->
            ( { model | pageModel = NotFoundModel }
            , Cmd.none
            )


view : Model -> Document Msg
view model =
    let
        content =
            if not model.appState.valid then
                misconfigured

            else
                case model.pageModel of
                    IndexModel indexModel ->
                        Html.map IndexMsg <| Index.view indexModel

                    KMDetailModel kmDetailModel ->
                        Html.map KMDetailMsg <| KMDetail.view kmDetailModel

                    SignupModel signupModel ->
                        Html.map SignupMsg <| Signup.view signupModel

                    SignupConfirmationModel signupConfirmationModel ->
                        Html.map SignupConfirmationMsg <| SignupConfirmation.view signupConfirmationModel

                    LoginModel loginModel ->
                        Html.map LoginMsg <| Login.view loginModel

                    OrganizationDetailModel organizationDetailModel ->
                        Html.map OrganizationDetailMsg <| OrganizationDetail.view organizationDetailModel

                    NotFoundModel ->
                        div [] [ text "Not found" ]

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
            case appState.credentials of
                Just credentials ->
                    loggedInHeaderNavigation credentials

                Nothing ->
                    publicHeaderNavigation
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


loggedInHeaderNavigation : AppState.Credentials -> Html Msg
loggedInHeaderNavigation credentials =
    div []
        [ ul [ class "nav navbar-nav ml-auto" ]
            [ li [ class "nav-item" ]
                [ a
                    [ href <| Routing.toString <| Routing.OrganizationDetail credentials.organizationId
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


misconfigured : Html msg
misconfigured =
    div [ class "alert alert-danger" ]
        [ h4 [ class "alert-heading" ] [ text "Configuration Error" ]
        , p [] [ text "Application is not configured correctly and cannot run." ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
