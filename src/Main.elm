import Html exposing (..)
import Html.App as App
import Html.Events exposing (..)
import Html.Attributes exposing (..)

import Routing
import Routing exposing (Route(..))
import Navigation


-- MODEL

type alias Model =
  { authCode : String
  , accessToken: String
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
  Model "" "" Routing.RootRoute
    |> urlUpdate maybeCode     


urlUpdate : Maybe String -> Model -> (Model, Cmd Msg)
urlUpdate maybeCode model =
  let 
    route = Routing.routeFromMaybe maybeCode
  in
    ({ model | route = route }, Cmd.none)


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

    AuthCodeRoute authCode ->
      div []
        [ text authCode ]


-- MESSAGES

type Msg = GetAccessToken


-- UPDATE

--getAccessTokenCmd : Model -> Cmd Msg
--getAccessTokenCmd model =


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GetAccessToken ->
      (model, Cmd.none)


