module Routes exposing (..)

import Types exposing (..)
import Api
import Dict
import UrlParser
import Navigation
import Hop
import Hop.Types exposing (Config, Address, Query)


routes : UrlParser.Parser (Route -> a) a
routes =
  UrlParser.oneOf
    [ UrlParser.format Home (UrlParser.s "")
    , UrlParser.format NewProposalRoute (UrlParser.s "new-proposal")
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


urlUpdate : ( Route, Address ) -> Model -> ( Model, Cmd Msg )
urlUpdate ( route, address ) model =
  ( { model | route = route, address = address }, Cmd.none )


checkForAuthCode : Address -> Cmd Msg
checkForAuthCode address =
  let
    authCode =
      address.query |> Dict.get "code"
  in
    case authCode of
      Just code ->
        Api.authenticateCmd code ApiMsg

      Nothing ->
        Cmd.none
