import Html exposing (..)
import Html.App as App
import Html.Events exposing (..)
import Html.Attributes exposing (..)

import Routing
import Routing exposing (Route(..))
import Navigation

import Http
import Task
import Json.Decode as Decode
import Json.Decode exposing (Decoder)

-- MODEL

type alias Model =
  { accessToken: Result String String
  , route : Routing.Route
  }


main : Program Never
main =
  Navigation.program Routing.parser
    { init = init
    , update = update
    , urlUpdate = urlUpdate
    , subscriptions = \_ -> Sub.none
    , view = view
    }


init : Maybe String -> (Model, Cmd Msg)
init maybeCode = 
  { accessToken = Err "Not yet fetched", route = Routing.RootRoute }
    |> urlUpdate maybeCode     


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
        (newModel, authenticateCmd authCode)

      RootRoute ->
        (newModel, Cmd.none)


apiRoot : String
apiRoot = "http://localhost:5000"


accessTokenDecoder : Decoder String
accessTokenDecoder =
  Decode.at ["access_token"] Decode.string


authenticateCmd : String -> Cmd Msg
authenticateCmd authCode =
  let
    body =
      "{\"auth_code\": \"" ++ authCode ++ "\"}"
        |> Http.string
    
    requestTask =
      Http.post accessTokenDecoder (apiRoot ++ "/token") body
  in
    Task.perform AuthFailed GotAccessToken requestTask

-- VIEW

-- TODO: move client_id and redirect_uri into environment "variables" or whatever you'd
-- call them in Elm
fbAuthUrl : String
fbAuthUrl = "https://www.facebook.com/dialog/oauth?client_id=1583083701926004&redirect_uri=http://localhost:3000/"


view : Model -> Html Msg
view model =
  case model.route of
    RootRoute ->
      div []
        [ a [ href fbAuthUrl ] [ text "Login with Facebook" ] ]

    AuthCodeRoute _ ->
      case model.accessToken of
        Err e ->
          div []
            [ text <| "Access token not available: " ++ e ]

        Ok accessToken ->
          div []
            [ text <| "Access token: " ++ accessToken ]

-- MESSAGES

type Msg
  = GotAccessToken String
  | AuthFailed Http.Error


-- UPDATE

--getAccessTokenCmd : Model -> Cmd Msg
--getAccessTokenCmd model =


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotAccessToken accessToken ->
      ({ model | accessToken = Ok accessToken }, Cmd.none)

    AuthFailed httpError ->
      ({ model | accessToken = Err (toString httpError) }, Cmd.none)

