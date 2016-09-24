import Html exposing (..)
import Html.App as App
import Html.Events exposing (..)
import Html.Attributes exposing (..)

import Material
import Material.Scheme
import Material.Button as Button
import Material.Options exposing (css)
import Material.Layout as Layout
import Material.Color as Color

import Form exposing (Form)
import Form.Validate as Validate exposing (..)
import Form.Input as Input

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
  | NewProposalRoute
  | FacebookRedirect
  | NotFoundRoute


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
  , form : Form () Proposal
  , mdl : Material.Model
  }


initialModel : Route -> Address -> Model
initialModel route address = 
  Model route address "" Nothing { name = "" } (Form.initial [] validate) Material.model


type alias Proposal =
  { title : String
  , body : String
  }



-- UPDATE


type Msg
  = ApiMsg Api.Msg
  | FormMsg Form.Msg
  | NoOp
  | Mdl (Material.Msg Msg)


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

    FormMsg formMsg ->
      ({ model | form = Form.update formMsg model.form }, Cmd.none)

    NoOp ->
      ( model, Cmd.none )

    Mdl msg' ->
      Material.update msg' model


validate : Validation () Proposal
validate =
  form2 Proposal
    (get "title" string)
    (get "body" string)



-- VIEW


type alias Mdl =
  Material.Model


view : Model -> Html Msg
view model =
  Material.Scheme.topWithScheme Color.Teal Color.LightGreen <|
    Layout.render Mdl
      model.mdl
      [ Layout.fixedHeader
      ]
      { header = [ h1 [ style [ ( "padding", ".5rem" ) ] ] [ text "Participate!" ] ]
      , drawer = []
      , tabs = ( [], [] )
      , main = [ viewBody model ]
      }



viewBody : Model -> Html Msg
viewBody model =
  case model.route of
    Home ->
      if String.isEmpty model.accessToken == True then
        div []
          [ a [ href Api.facebookAuthUrl ] [ text "Login with Facebook" ] ]
      else
        div []
          [ 
            text <| "Hello, " ++ ( .name model.me )
            ,
            h3 []
              [ a [ href "/new-proposal" ] [ text "Create a proposal" ] ]
          ]

    NewProposalRoute ->
      div []
        [ 
          h2 []
            [ text <| "New Proposal" ]
          ,

          App.map FormMsg (formView model.form)
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


formView : Form () Proposal -> Html Form.Msg
formView form =
  let
    errorFor field =
      case field.liveError of
        Just error ->
          div [ class "error" ] [ text (toString error) ]

        Nothing ->
          text ""

    title = Form.getFieldAsString "title" form
    body = Form.getFieldAsString "body" form
  in
    div []
      [ label [] [ text "Title" ]
      , Input.textInput title []
      , errorFor title

      , label [] [ text "Body" ]
      , Input.textArea body []
      , errorFor body

      , button
          [ onClick Form.Submit ]
          [ text "Submit" ]
      ]



-- APP


init : ( Route, Address ) -> ( Model, Cmd Msg )
init ( route, address ) =
  ( initialModel route address, checkForAuthCode address)

 
main : Program Never
main =
  Navigation.program urlParser
    { init = init
    , update = update
    , urlUpdate = urlUpdate
    , subscriptions = (always Sub.none)
    , view = view
    }
