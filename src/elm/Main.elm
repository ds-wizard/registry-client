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
    , screenModel : ScreenModel
    }


type ScreenModel
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
        , screenModel = NotFoundModel
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.screenModel ) of
        ( LinkedClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        ( UrlChanged url, _ ) ->
            initChildModel { model | route = Routing.toRoute url }

        ( IndexMsg indexMsg, IndexModel indexModel ) ->
            ( { model | screenModel = IndexModel <| Index.update indexMsg indexModel }
            , Cmd.none
            )

        ( KMDetailMsg kmDetailMsg, KMDetailModel kmDetailModel ) ->
            ( { model | screenModel = KMDetailModel <| KMDetail.update kmDetailMsg kmDetailModel }
            , Cmd.none
            )

        ( SignupMsg signupMsg, SignupModel signupModel ) ->
            let
                ( newSignupModel, cmd ) =
                    Signup.update signupMsg model.appState signupModel
            in
            ( { model | screenModel = SignupModel newSignupModel }
            , Cmd.map SignupMsg cmd
            )

        ( SignupConfirmationMsg confirmSignupMsg, SignupConfirmationModel signupConfirmationModel ) ->
            ( { model | screenModel = SignupConfirmationModel <| SignupConfirmation.update confirmSignupMsg signupConfirmationModel }
            , Cmd.none
            )

        ( LoginMsg loginMsg, LoginModel loginModel ) ->
            let
                ( newLoginModel, cmd ) =
                    Login.update loginMsg loginModel
            in
            ( { model | screenModel = LoginModel <| newLoginModel }
            , Cmd.map LoginMsg cmd
            )

        ( OrganizationDetailMsg organizationDetailMsg, OrganizationDetailModel organizationDetailModel ) ->
            let
                ( newOrganizationDetailModel, cmd ) =
                    OrganizationDetail.update organizationDetailMsg organizationDetailModel
            in
            ( { model | screenModel = OrganizationDetailModel newOrganizationDetailModel }
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
            ( { model | screenModel = IndexModel indexModel }
            , Cmd.map IndexMsg indexCmd
            )

        Routing.KMDetail pkgId ->
            let
                ( kmDetailModel, kmDetailCmd ) =
                    KMDetail.init model.appState pkgId
            in
            ( { model | screenModel = KMDetailModel kmDetailModel }
            , Cmd.map KMDetailMsg kmDetailCmd
            )

        Routing.Signup ->
            ( { model | screenModel = SignupModel Signup.init }
            , Cmd.none
            )

        Routing.SignupConfirmation organizationId hash ->
            let
                ( signupConfirmationModel, signupConfirmationCmd ) =
                    SignupConfirmation.init model.appState organizationId hash
            in
            ( { model | screenModel = SignupConfirmationModel signupConfirmationModel }
            , Cmd.map SignupConfirmationMsg signupConfirmationCmd
            )

        Routing.Login ->
            ( { model | screenModel = LoginModel Login.init }
            , Cmd.none
            )

        Routing.OrganizationDetail _ ->
            ( { model | screenModel = OrganizationDetailModel OrganizationDetail.init }
            , Cmd.none
            )

        Routing.NotFound ->
            ( { model | screenModel = NotFoundModel }
            , Cmd.none
            )


view : Model -> Document Msg
view model =
    let
        content =
            if not model.appState.valid then
                misconfigured

            else
                case model.screenModel of
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
