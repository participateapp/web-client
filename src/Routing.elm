port module Routing exposing (urlParser, newRoute)

import Navigation
import UrlParser exposing ((</>))
import Hop
import Hop.Types exposing (Config, Address, Query)
import Types exposing (..)


routes : UrlParser.Parser (Route -> a) a
routes =
    UrlParser.oneOf
        [ UrlParser.format HomeRoute (UrlParser.s "")
        , UrlParser.format NewProposalRoute (UrlParser.s "new-proposal")
        , UrlParser.format ProposalRoute (UrlParser.s "proposals" </> UrlParser.string)
        , UrlParser.format FacebookRedirectRoute (UrlParser.s "facebook_redirect")
        ]


hopConfig : Config
hopConfig =
    { basePath = ""
    , hash = False
    }


urlParser : Navigation.Parser ( Route, Address )
urlParser =
    let
        parse path =
            path
                |> UrlParser.parse identity routes
                |> Result.withDefault NotFoundRoute

        resolver =
            Hop.makeResolver hopConfig parse
    in
        Navigation.makeParser (.href >> resolver)


urlBuilder : Route -> String
urlBuilder route =
    "/"
        ++ case route of
            HomeRoute ->
                ""

            NewProposalRoute ->
                "new-proposal"

            ProposalRoute id ->
                "proposals/" ++ id

            FacebookRedirectRoute ->
                "facebook_redirect"

            NotFoundRoute ->
                ""


newRoute : Route -> Cmd msg
newRoute route =
    Navigation.newUrl <| urlBuilder route
