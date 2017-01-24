port module Routing exposing (urlParser)

import Navigation
import UrlParser exposing ((</>))
import Hop
import Hop.Types exposing (Config, Address, Query)
import Types exposing (..)


routes : UrlParser.Parser (Route -> a) a
routes =
    UrlParser.oneOf
        [ UrlParser.format Home (UrlParser.s "")
        , UrlParser.format NewProposalRoute (UrlParser.s "new-proposal")
        , UrlParser.format ProposalRoute (UrlParser.s "proposals" </> UrlParser.string)
        , UrlParser.format FacebookRedirect (UrlParser.s "facebook_redirect")
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
