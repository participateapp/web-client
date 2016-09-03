module Routing exposing (..)

import String
import Dict exposing (Dict)

import Navigation
import Erl
 

type Route
    = RootRoute
    | AuthCodeRoute String


authCodeExtractor : Navigation.Location -> Maybe String
authCodeExtractor location =
    -- are we doing navigation through hashes
    location.href
        |> Erl.parse
        |> .query
        |> Dict.get "code"


parser : Navigation.Parser (Maybe String)
parser =
    Navigation.makeParser authCodeExtractor


routeFromMaybe : Maybe String -> Route
routeFromMaybe maybeCode =
    case maybeCode of
        Just code ->
            AuthCodeRoute code

        Nothing ->
            RootRoute
