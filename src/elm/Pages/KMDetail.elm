module Pages.KMDetail exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Common.AppState exposing (AppState)
import Common.Entities.ApiError as ApiError exposing (ApiError)
import Common.Entities.OrganizationInfo exposing (OrganizationInfo)
import Common.Entities.PackageDetail exposing (PackageDetail)
import Common.Requests as Requests
import Common.View.ItemIcon as ItemIcon
import Common.View.Page as Page
import Html exposing (Html, a, br, code, div, h5, li, p, strong, text, ul)
import Html.Attributes exposing (class, href, target)
import Markdown
import Routing
import Version


init : AppState -> String -> ( Model, Cmd Msg )
init appState packageId =
    ( { package = Loading }
    , Requests.getPackage appState packageId GetPackageCompleted
    )



-- MODEL


type alias Model =
    { package : ActionResult PackageDetail }


setPackage : ActionResult PackageDetail -> Model -> Model
setPackage package model =
    { model | package = package }



-- UPDATE


type Msg
    = GetPackageCompleted (Result ApiError PackageDetail)


update : Msg -> Model -> Model
update msg =
    case msg of
        GetPackageCompleted result ->
            ActionResult.apply setPackage (ApiError.toActionResult "Unable to get package.") result



-- VIEW


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
            , viewOrganization package.organization
            ]

        viewLicense =
            [ h5 [] [ text "License" ]
            , p []
                [ a [ href <| "https://spdx.org/licenses/" ++ package.license ++ ".html", target "_blank" ]
                    [ text package.license ]
                ]
            ]

        viewCurrentVersion =
            [ h5 [] [ text "Version" ]
            , p [] [ text <| Version.toString package.version ]
            ]

        otherVersions =
            package.versions
                |> List.filter ((/=) package.version)
                |> List.sortWith Version.compare
                |> List.reverse

        viewOtherVersions =
            case otherVersions of
                [] ->
                    []

                versions ->
                    [ h5 [] [ text "Other versions" ]
                    , ul []
                        (List.map viewVersion versions)
                    ]

        viewVersion version =
            li []
                [ a [ href <| Routing.toString <| Routing.KMDetail (package.organization.organizationId ++ ":" ++ package.kmId ++ ":" ++ Version.toString version) ]
                    [ text <| Version.toString version ]
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
                    ++ viewLicense
                    ++ viewCurrentVersion
                    ++ viewOtherVersions
                    ++ viewSupportedMetamodel
                    ++ viewParentKnowledgeModel
                )
            ]
        ]


viewOrganization : OrganizationInfo -> Html msg
viewOrganization organization =
    div [ class "organization" ]
        [ ItemIcon.view { text = organization.name, image = organization.logo }
        , div [ class "content" ]
            [ strong [] [ text organization.name ]
            , br [] []
            , text organization.organizationId
            ]
        ]
