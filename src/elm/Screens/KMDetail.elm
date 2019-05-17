module Screens.KMDetail exposing
    ( Model
    , Msg(..)
    , init
    , initEmpty
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Common.AppState exposing (AppState)
import Common.Entities.PackageDetail exposing (PackageDetail)
import Common.Requests as Requests
import Common.View.Page as Page
import Html exposing (Html, a, code, div, h5, li, p, text, ul)
import Html.Attributes exposing (class, href)
import Http
import Markdown
import Routing


type alias Model =
    { package : ActionResult PackageDetail }


type Msg
    = GetPackageCompleted (Result Http.Error PackageDetail)


initEmpty : Model
initEmpty =
    { package = Loading }


init : AppState -> String -> ( Model, Cmd Msg )
init appState packageId =
    ( initEmpty
    , Requests.getPackage appState packageId GetPackageCompleted
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetPackageCompleted result ->
            case result of
                Ok package ->
                    ( { model | package = Success package }, Cmd.none )

                Err _ ->
                    ( { model | package = Error "Unable to get package." }, Cmd.none )


view : Model -> Html Msg
view model =
    Page.actionResultView viewDetail model.package


viewDetail : PackageDetail -> Html Msg
viewDetail package =
    let
        viewKmId =
            [ h5 [] [ text "Knowledge Model ID" ]
            , p []
                [ code [] [ text package.id ] ]
            ]

        viewPublishedBy =
            [ h5 [] [ text "Published by" ]
            , p [] [ text package.organization.name ]
            ]

        viewCurrentVersion =
            [ h5 [] [ text "Version" ]
            , p [] [ text package.version ]
            ]

        viewOtherVersions =
            case List.filter (not << (==) package.version) package.versions of
                [] ->
                    []

                versions ->
                    [ h5 [] [ text "Other versions" ]
                    , ul []
                        (List.map viewVersion <| List.reverse versions)
                    ]

        viewVersion version =
            li []
                [ a [ href <| Routing.toString <| Routing.KMDetail (package.organization.organizationId ++ ":" ++ package.kmId ++ ":" ++ version) ]
                    [ text version ]
                ]

        viewSupportedMetamodel =
            [ h5 [] [ text "Supported metamodel" ]
            , p [] [ text <| "Since " ++ String.fromInt package.metamodelVersion ]
            ]

        viewParentKnowledgeModel =
            case package.parentPackageId of
                Just parentPackageId ->
                    [ h5 [] [ text "Parent Knowledge Model" ]
                    , p []
                        [ a [ href <| Routing.toString <| Routing.KMDetail parentPackageId ]
                            [ text parentPackageId
                            ]
                        ]
                    ]

                Nothing ->
                    []
    in
    div [ class "KMDetail" ]
        [ div [ class "row" ]
            [ div [ class "col-12 col-md-8" ]
                [ Markdown.toHtml [] package.readme ]
            , div [ class "KMDetail__Panel col-12 col-md-4" ]
                (viewKmId
                    ++ viewPublishedBy
                    ++ viewCurrentVersion
                    ++ viewOtherVersions
                    ++ viewSupportedMetamodel
                    ++ viewParentKnowledgeModel
                )
            ]
        ]
