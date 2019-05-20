module Pages.KMDetail exposing
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


setPackage : ActionResult PackageDetail -> Model -> Model
setPackage package model =
    { model | package = package }


initEmpty : Model
initEmpty =
    { package = Loading }


init : AppState -> String -> ( Model, Cmd Msg )
init appState packageId =
    ( initEmpty
    , Requests.getPackage appState packageId GetPackageCompleted
    )


type Msg
    = GetPackageCompleted (Result Http.Error PackageDetail)


update : Msg -> Model -> Model
update msg =
    case msg of
        GetPackageCompleted result ->
            ActionResult.apply setPackage "Unable to get package." result


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
            [ h5 [] [ text "Metamodel version" ]
            , p [] [ text <| String.fromInt package.metamodelVersion ]
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
