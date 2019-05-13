module Main exposing (main)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Html exposing (Html, a, div, img, text)
import Html.Attributes exposing (class, href, src)
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


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { route = Routing.toRoute url
      , key = key
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
                    Signup.update signupMsg model.signupModel
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
            case model.route of
                Routing.Index ->
                    Html.map IndexMsg <| Index.view model.indexModel

                Routing.KMDetail _ _ ->
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
            , div [ class "ui main text container" ]
                [ content ]
            ]
    in
    { title = "Registry"
    , body = html
    }


header : Html Msg
header =
    div [ class "ui fixed inverted menu" ]
        [ div [ class "ui container" ]
            [ a [ href <| Routing.toString Routing.Index, class "header item" ]
                [ img [ class "logo", src "/img/logo.svg" ] []
                , text "Registry"
                ]
            , div [ class "right menu" ]
                [ a [ href <| Routing.toString Routing.Login, class " item" ]
                    [ text "Log in" ]
                , a [ href <| Routing.toString Routing.Signup, class " item" ]
                    [ text "Sign up" ]
                ]
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
