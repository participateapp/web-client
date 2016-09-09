import Html exposing (..)
import Html.App as App
import Html.Events exposing (..)
import Html.Attributes exposing (..)

import Routing
import Routing exposing (Route(..))
import Navigation

import Http
import Task exposing (Task)
import Json.Decode as Decode
import Json.Decode exposing (Decoder)

import Api


main : Program Never
main =
  Navigation.program Routing.parser
    { init = init
    , update = update
    , urlUpdate = urlUpdate
    , subscriptions = \_ -> Sub.none
    , view = view
    }

-- MODEL

type alias Model =
  { accessToken: Result String String
  , route : Routing.Route
  }


init : Maybe String -> (Model, Cmd Msg)
init maybeCode = 
  { accessToken = Err "Not yet fetched", route = Routing.RootRoute }
    |> urlUpdate maybeCode     


-- VIEW


view : Model -> Html Msg
view model =
  case model.route of
    RootRoute ->
      div []
        [ a [ href Api.fbAuthUrl ] [ text "Login with Facebook" ] ]

    AuthCodeRoute _ ->
      case model.accessToken of
        Err e ->
          div []
            [ text <| "Access token not available: " ++ e ]

        Ok accessToken ->
          div []
            [ text <| "Access token: " ++ accessToken ]


-- UPDATE

type Msg
  = ApiMsg Api.Msg


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ApiMsg apiMsg ->
      case apiMsg of
        Api.GotAccessToken accessToken ->
          ({ model | accessToken = Ok accessToken }, Cmd.none)

        Api.AuthFailed httpError ->
          ({ model | accessToken = Err (toString httpError) }, Cmd.none)


urlUpdate : Maybe String -> Model -> (Model, Cmd Msg)
urlUpdate maybeCode model =
  let
    route =
      Routing.routeFromMaybe maybeCode

    newModel =
      { model | route = route }
  in
    case route of
      AuthCodeRoute authCode ->
        (newModel, Api.authenticateCmd ApiMsg authCode)

      RootRoute ->
        (newModel, Cmd.none)
