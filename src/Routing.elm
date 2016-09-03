module Routing exposing (..)

import String
import Dict exposing (Dict)

import Navigation
import UrlParameterParser exposing (parseParams)
 

type Route
    = LoginRoute
    | NotFoundRoute

matchers : Parser (Route -> a) a
matchers =
    oneOf
        [ format LoginRoute (s "")
        ]


-- TODO: parse query params
-- ...and add it to the model?
locationParser : Navigation.Location -> Result String Route
locationParser location =
    -- are we doing navigation through hashes
    location.hash
        |> String.dropLeft 1
        |> parse identity matchers


parser : Navigation.Parser (Result String Route)
parser =
    Navigation.makeParser locationParser


routeFromResult : Result String Route -> Route
routeFromResult result =
    case result of
        Ok route ->
            route

        Err string ->
            NotFoundRoute
