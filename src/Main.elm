import Html exposing (..)
import Html.App as App
import Html.Events exposing (..)
import Html.Attributes exposing (..)

import Routing


-- MODEL

type alias Model =
  { authCode : String
  , accessToken: String
  }


main : Program Never
main =
  App.program
    { init = Model "" "" ! []
    , update = update
    , subscriptions = \_ -> Sub.none
    , view = view
    }


-- VIEW

-- TODO: move client_id and redirect_uri into environment "variables" or whatever you'd
-- call them in Elm
fbAuthUrl : String
fbAuthUrl = "https://www.facebook.com/dialog/oauth?client_id=1583083701926004&redirect_uri=http://localhost:3000/"

view : Model -> Html Msg
view model = 
  div []
    [ a [ href fbAuthUrl ] [ text "Login with Facebook" ] ]


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


