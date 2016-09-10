import Html exposing (..)
import Html.App as App
import Html.Events exposing (..)
import Html.Attributes exposing (..)

import Routing
import Navigation
import Api


main : Program Never
main =
    Navigation.program (Navigation.makeParser Routing.locFor)
        { init = init
        , update = update
        , urlUpdate = updateRoute
        , subscriptions = \_ -> Sub.none
        , view = view
        }


updateRoute : Maybe Routing.Location -> Model -> ( Model, Cmd Msg )
updateRoute route model =
    let
        newModel =
            { model | route = route }
    in
        case route of
            Routing.AuthCodeRedirect authCode ->
                (newModel, Api.authenticateCmd ApiMsg authCode)

            Routing.Home ->
                (newModel, Cmd.none)



-- MODEL

type alias Model =
  { accessToken: String
  , error : Maybe String
  , userInfo : Api.UserInfo
  , route : Routing.Model
  }


init : Maybe Routing.Location -> ( Model, Cmd Msg )
init location =
    let
        route =
            Routing.init location
    in
        { route = route
        }
            ! []


-- VIEW


view : Model -> Html Msg
view model =
    case model.route of
      Routing.Home ->
          case model.accessToken of
              Just a ->
                  div [] 
                      [ text <| "Hello World" ]
              Nothing ->
                  div []
                      [ a [ href Api.fbAuthUrl ] [ text "Login with Facebook" ] ]

      Routing.AuthCodeRedirect _ ->
          case model.error of
              Just e ->
                  div []
                      [ text <| "Access token not available: " ++ e ]

              Nothing ->
                 div []
                    [ text <| "Access token: " ++ model.accessToken ]



-- UPDATE

type Msg
  = ApiMsg Api.Msg


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ApiMsg apiMsg ->
            case apiMsg of
                Api.GotAccessToken accessToken ->
                    ({ model | accessToken = accessToken }, Api.getMeCmd ApiMsg accessToken)

                Api.AuthFailed httpError ->
                    ({ model | error = Just <| toString httpError }, Cmd.none)

                Api.GotUserInfo userInfo ->
                    ({ model | userInfo = userInfo }, Cmd.none)
