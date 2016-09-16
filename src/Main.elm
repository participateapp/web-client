import Html exposing (..)
import Html.App as App
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Dict
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
  { hash = False 
  , basePath = ""
  }


-- taken from https://github.com/sporto/hop/blob/master/examples/basic/Main.elm
urlParser : Navigation.Parser ( Route, Address )
urlParser =
  let
    -- A parse function takes the normalised path from Hop after taking
    -- in consideration the basePath and the hash.
    -- This function then returns a result.
    parse path =
      -- First we parse using UrlParser.parse.
      -- Then we return the parsed route or NotFoundRoute if the parsed failed.
      -- You can choose to return the parse return directly.
      path
        |> UrlParser.parse identity routes
        |> Result.withDefault NotFoundRoute

    resolver =
      -- Create a function that parses and formats the URL
      -- This function takes 2 arguments: The Hop Config and the parse function.
      Hop.makeResolver hopConfig parse
  in
    -- Create a Navigation URL parser
    Navigation.makeParser (.href >> resolver)


urlUpdate : ( Route, Address ) -> Model -> ( Model, Cmd Msg )
urlUpdate ( route, address ) model =
  ( { model | route = route, address = address }, Cmd.none )


-- MODEL


type alias Model =
  { accessToken: String
  , error : Maybe String
  , userInfo : Api.UserInfo
  , route : Route
  , address: Address
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

        Api.GotUserInfo userInfo ->
          ({ model | userInfo = userInfo }, Cmd.none)


-- VIEW


view : Model -> Html Msg
view model =
  case model.route of
    Home ->
      case model.accessToken of
        Just a ->
          div [] 
            [ text <| "Hello World" ]
        Nothing ->
          div []
            [ a [ href Api.facebookAuthUrl ] [ text "Login with Facebook" ] ]


-- APP


init : ( Route, Address ) -> ( Model, Cmd Msg )
init ( route, address ) = 
  ( Model address route "" "" "", Cmd.none)


main : Program Never
main =
  Navigation.program urlParser
    { init = init
    , update = update
    , urlUpdate = urlUpdate
    , subscriptions = (always Sub.none)
    , view = view
    }
