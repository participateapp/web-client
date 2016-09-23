import Html exposing (..)
import Html.App as App
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Dict
import String
import Navigation
import UrlParser exposing ((</>))
import Hop
import Hop.Types exposing (Config, Address, Query)
import Api


-- ROUTES


type Route
  = Home
  | FacebookRedirect
  | NotFoundRoute


routes : UrlParser.Parser (Route -> a) a
routes =
  UrlParser.oneOf
    [ UrlParser.format Home (UrlParser.s "")
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
    authCode = address.query |> Dict.get "code"
  in
    case authCode of
      Just code -> 
        Api.authenticateCmd ApiMsg code

      Nothing -> Cmd.none




-- MODEL


type alias Model =
  { route : Route
  , address: Address
  , accessToken: String
  , error : Maybe String
  , me : Api.Me
  }



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

        Api.GotMe me ->
          ({ model | me = me}, Navigation.newUrl "/")



-- VIEW


view : Model -> Html Msg
view model =
  case model.route of
    Home ->
      if String.isEmpty model.accessToken == True then
        div []
          [ a [ href Api.facebookAuthUrl ] [ text "Login with Facebook" ] ]
      else
        div []
          [ 
            text <| "Hello, " ++ ( .name model.me ),

            h3 []
              [ a [ href "/new-proposal" ] [ text "Create a proposal" ] ]
          ]
        

    NotFoundRoute ->
      div []
        [ text <| "Not found" ]

    FacebookRedirect ->
      div []
        [ text <| "Authenticating, please wait..." ]

      -- facebook auth debug
      --let
      --  authCode = model.address.query |> Dict.get "code"

      --in
      --  case authCode of
      --    Just code ->
      --      div []
      --        [ text <| "Redirect from facebook. Auth code: " ++ code ]

      --    Nothing ->
      --      div []
      --        [ text <| "Redirect from facebook. But no code somehow :("]



-- APP


init : ( Route, Address ) -> ( Model, Cmd Msg )
init ( route, address ) =
  ( Model route address "" Nothing { name = "" }, checkForAuthCode address)

 
main : Program Never
main =
  Navigation.program urlParser
    { init = init
    , update = update
    , urlUpdate = urlUpdate
    , subscriptions = (always Sub.none)
    , view = view
    }
