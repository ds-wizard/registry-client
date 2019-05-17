module Routing exposing (Route(..), toRoute, toString)

import Url
import Url.Parser exposing ((</>), Parser, map, oneOf, parse, s, string, top)


type Route
    = Index
    | KMDetail String
    | Signup
    | ConfirmSignup String String
    | Login
    | OrganizationDetail String
    | NotFound


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Index top
        , map KMDetail (s "km" </> string)
        , map Signup (s "signup")
        , map ConfirmSignup (s "signup" </> string </> string)
        , map Login (s "login")
        , map OrganizationDetail (s "organization" </> string)
        ]


toRoute : Url.Url -> Route
toRoute url =
    Maybe.withDefault NotFound (parse routeParser url)


toString : Route -> String
toString route =
    case route of
        Index ->
            "/"

        KMDetail pkgId ->
            "/km/" ++ pkgId

        Signup ->
            "/signup"

        ConfirmSignup orgId hash ->
            "/signup/" ++ orgId ++ "/" ++ hash

        Login ->
            "/login"

        OrganizationDetail orgId ->
            "/organization/" ++ orgId

        _ ->
            "/"
