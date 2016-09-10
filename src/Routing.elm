module Routing exposing (..)

import String exposing (split)
import Dict exposing (Dict)

import Navigation
import Erl


type Location
    = Home
    | AuthCodeRedirect String


type alias Model =
    Maybe Location


init : Maybe Location -> Model
init location =
    location


urlFor : Location -> String
urlFor loc =
  let
    url =
      case loc of
        Home ->
            "/"
        AuthCodeRedirect _ ->
            "/"
  in
    "#" ++ url


locFor : Navigation.Location -> Maybe Location
locFor location =
  let
    authCode = 
      location.href
        |> Erl.parse
        |> .query
        |> Dict.get "code"

    segments =
      location.hash
        |> split "/"
        |> List.filter (\seg -> seg /= "" && seg /= "#")
  in
    case authCode of
      Just code ->
        Just (AuthCodeRedirect code)

      Nothing ->
        case segments of
          [] ->
            Just Home

          _ ->
            Nothing

