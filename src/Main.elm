port module Main exposing (main)

import Html exposing (..)
import Html.App as App
import Html.Events exposing (..)
import Html.Attributes exposing (style, href, class)
import Material
import Material.Scheme
import Material.Button as Button
import Material.Textfield as Textfield
import Material.List as List
import Material.Options exposing (css)
import Material.Options as Options
import Material.Layout as Layout
import Material.Color as Color
import Material.Elevation as Elevation
import Material.Grid exposing (grid, size, cell, Device(..))
import Form exposing (Form)
import Form.Field
import Form.Input
import Form.Error
import Form.Validate exposing (Validation, form1, form2, get, string)
import Dict exposing (Dict)
import Set exposing (Set)
import String
import Navigation
import UrlParser exposing ((</>))
import Hop
import Hop.Types exposing (Config, Address, Query)
import Types exposing (..)
import Api


-- ROUTES


type Route
    = Home
    | NewProposalRoute
    | ProposalRoute String
    | FacebookRedirect
    | NotFoundRoute


routes : UrlParser.Parser (Route -> a) a
routes =
    UrlParser.oneOf
        [ UrlParser.format Home (UrlParser.s "")
        , UrlParser.format NewProposalRoute (UrlParser.s "new-proposal")
        , UrlParser.format ProposalRoute (UrlParser.s "proposals" </> UrlParser.string)
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
    let
        model1 =
            { model | route = route, address = address }

        _ =
            Debug.log "urlUpdate" ( route, address )
    in
        case route of
            ProposalRoute id ->
                case Dict.get id model.proposals of
                    Nothing ->
                        ( model1
                        , Api.getProposal id model.accessToken ApiMsg
                        )

                    Just _ ->
                        ( model1, Cmd.none )

            Home ->
                ( model1
                , Api.getProposalList model.accessToken ApiMsg
                )

            _ ->
                ( model1, Cmd.none )


checkForAuthCode : Address -> Cmd Msg
checkForAuthCode address =
    let
        authCode =
            address.query |> Dict.get "code"
    in
        case authCode of
            Just code ->
                Api.authenticate code ApiMsg

            Nothing ->
                Cmd.none



-- Port for storage of accessToken


port storeAccessToken : String -> Cmd msg



-- MODEL


type alias Model =
    { route : Route
    , address : Address
    , accessToken : String
    , error : Maybe String
    , me : Me
    , form : Form () NewProposal
    , mdl : Material.Model
    , proposals : Dict String Proposal
    }


initialModel : String -> Route -> Address -> Model
initialModel accessToken route address =
    { route = route
    , address = address
    , accessToken = accessToken
    , error = Nothing
    , me = { name = "" }
    , form = Form.initial [] validate
    , mdl = Material.model
    , proposals = Dict.empty
    }



-- UPDATE


type Msg
    = ApiMsg Api.Msg
    | NavigateToPath String
    | FormMsg Form.Msg
    | NoOp
    | Mdl (Material.Msg Msg)
    | SupportProposal String Bool


addProposal : Proposal -> Model -> Model
addProposal proposal model =
    { model | proposals = Dict.insert proposal.id proposal model.proposals }


addProposalList : ProposalList -> Model -> Model
addProposalList proposalList model =
    List.foldl addProposal model proposalList


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ApiMsg apiMsg ->
            case apiMsg of
                Api.GotAccessToken accessToken ->
                    ( { model | accessToken = accessToken }
                    , Cmd.batch
                        [ storeAccessToken accessToken
                        , Api.getMe accessToken ApiMsg
                        ]
                    )

                Api.AuthFailed httpError ->
                    ( { model | error = Just <| toString httpError }, Cmd.none )

                Api.GotMe me ->
                    ( { model | me = me }, Navigation.newUrl "/" )

                Api.ProposalCreated proposal ->
                    ( model
                        |> addProposal proposal
                    , Navigation.newUrl <|
                        Hop.output hopConfig { path = [ "proposals", proposal.id ], query = Dict.empty }
                    )

                Api.ProposalCreationFailed httpError ->
                    ( { model | error = Just <| toString httpError }, Cmd.none )

                Api.ProposalSupported id ->
                    ( model
                    , Navigation.newUrl <|
                        Hop.output hopConfig { path = [ "proposals", id ], query = Dict.empty }
                    )

                Api.SupportProposalFailed httpError ->
                    ( { model | error = Just <| toString httpError }, Cmd.none )

                Api.GotProposal proposal ->
                    ( model
                        |> addProposal proposal
                    , Cmd.none
                    )

                Api.GettingProposalFailed httpError ->
                    ( { model | error = Just <| toString httpError }, Cmd.none )

                Api.GotProposalList proposalList ->
                    ( model
                        |> addProposalList proposalList
                    , Cmd.none
                    )

                Api.GettingProposalListFailed httpError ->
                    ( { model | error = Just <| toString httpError }, Cmd.none )

        NavigateToPath path ->
            ( model
            , Navigation.newUrl <| Hop.outputFromPath hopConfig path
            )

        FormMsg formMsg ->
            case ( formMsg, Form.getOutput model.form ) of
                ( Form.Submit, Just proposalInput ) ->
                    model ! [ Api.createProposal proposalInput model.accessToken ApiMsg ]

                _ ->
                    ( { model | form = Form.update formMsg model.form }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

        Mdl msg' ->
            Material.update msg' model

        SupportProposal id newState ->
            ( model
            , Api.supportProposal id newState model.accessToken ApiMsg
            )


validate : Validation () NewProposal
validate =
    form2 NewProposal
        (get "title" string)
        (get "body" string)



-- VIEW


type alias Mdl =
    Material.Model


view : Model -> Html Msg
view model =
    Material.Scheme.topWithScheme Color.Amber Color.Red <|
        Layout.render Mdl
            model.mdl
            [ Layout.fixedHeader
            ]
            { header = [ h4 [ style [ ( "padding", ".5rem" ) ] ] [ text "Participate!" ] ]
            , drawer = []
            , tabs = ( [], [] )
            , main = [ div [ style [ ( "margin", "2rem" ) ] ] [ viewBody model ] ]
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
                    [ text <| "Hello, " ++ (.name model.me)
                    , h3 []
                        [ a [ onClick <| NavigateToPath "/new-proposal" ]
                            [ text "Create a proposal" ]
                        ]
                    , h3 [] [ text "Existing Proposals" ]
                    , div [] [ viewProposalList model ]
                    ]

        NewProposalRoute ->
            div []
                [ h2 []
                    [ text <| "New Proposal" ]
                , formView model
                ]

        ProposalRoute id ->
            div []
                [ h2 [] [ text "Proposal" ]
                , viewProposal model id
                ]

        NotFoundRoute ->
            div []
                [ text <| "Not found" ]

        FacebookRedirect ->
            div []
                [ text <| "Authenticating, please wait..." ]


formView : Model -> Html Msg
formView model =
    grid []
        [ cell [ size All 12 ] [ titleField model ]
        , cell [ size All 12 ] [ bodyField model ]
        , cell [ size All 12 ] [ submitButton model ]
        ]


titleField : Model -> Html Msg
titleField model =
    let
        title =
            Form.getFieldAsString "title" model.form

        conditionalProperties =
            case title.liveError of
                Just error ->
                    case error of
                        Form.Error.InvalidString ->
                            [ Textfield.error "Can't be blank" ]

                        Form.Error.Empty ->
                            [ Textfield.error "Can't be blank" ]

                        _ ->
                            [ Textfield.error <| toString error ]

                Nothing ->
                    []
    in
        Textfield.render Mdl
            [ 0, 0 ]
            model.mdl
            ([ Textfield.label "Title"
             , Textfield.floatingLabel
             , Textfield.text'
             , Textfield.value <| Maybe.withDefault "" title.value
             , Textfield.onInput <| FormMsg << (Form.Field.Text >> Form.Input title.path)
             , Textfield.onFocus <| FormMsg <| Form.Focus title.path
             , Textfield.onBlur <| FormMsg <| Form.Blur title.path
             ]
                ++ conditionalProperties
            )


bodyField : Model -> Html Msg
bodyField model =
    let
        body =
            Form.getFieldAsString "body" model.form

        conditionalProperties =
            case body.liveError of
                Just error ->
                    case error of
                        Form.Error.InvalidString ->
                            [ Textfield.error "Can't be blank" ]

                        Form.Error.Empty ->
                            [ Textfield.error "Can't be blank" ]

                        _ ->
                            [ Textfield.error <| toString error ]

                Nothing ->
                    []
    in
        Textfield.render Mdl
            [ 0, 1 ]
            model.mdl
            ([ Textfield.label "Body"
             , Textfield.floatingLabel
             , Textfield.textarea
             , Textfield.value <| Maybe.withDefault "" body.value
             , Textfield.onInput <| FormMsg << (Form.Field.Text >> Form.Input body.path)
             , Textfield.onFocus <| FormMsg <| Form.Focus body.path
             , Textfield.onBlur <| FormMsg <| Form.Blur body.path
             ]
                ++ conditionalProperties
            )


submitButton : Model -> Html Msg
submitButton model =
    Button.render Mdl
        [ 1 ]
        model.mdl
        [ Button.raised
        , Button.ripple
        , Color.background <| Color.color Color.Green Color.S500
        , Color.text <| Color.white
        , Button.onClick <| FormMsg <| Form.Submit
        ]
        [ text "Submit" ]


viewProposal : Model -> String -> Html Msg
viewProposal model id =
    case Dict.get id model.proposals of
        Nothing ->
            div [] [ text "Unknown proposal id: ", text id ]

        Just proposal ->
            div []
                [ div [] [ text "Title: ", text proposal.title ]
                , div [] [ text "Author: ", text proposal.author.name ]
                , div [] [ text "Body: ", text proposal.body ]
                , div [] [ text "Support Count: ", text <| toString proposal.supportCount ]
                , button [ onClick <| SupportProposal id (not proposal.supportedByMe) ]
                    [ text <|
                        if proposal.supportedByMe then
                            "Unsupport this proposal (unimplemented)"
                        else
                            "Support this proposal"
                    ]
                ]


viewProposalList : Model -> Html Msg
viewProposalList model =
    ul [] <|
        List.map
            viewProposalListEntry
            (Dict.values model.proposals)


viewProposalListEntry : Proposal -> Html Msg
viewProposalListEntry proposal =
    li []
        [ i [] [ text proposal.author.name ]
        , text ": "
        , a
            [ onClick <| NavigateToPath <| "proposals/" ++ proposal.id ]
            [ text proposal.title ]
        ]



-- APP


type alias Flags =
    { accessToken : Maybe String }


init : Flags -> ( Route, Address ) -> ( Model, Cmd Msg )
init flags ( route, address ) =
    let
        model0 =
            initialModel (Maybe.withDefault "" flags.accessToken) route address

        ( model1, cmd1 ) =
            urlUpdate ( route, address ) model0
    in
        ( model1, Cmd.batch [ cmd1, checkForAuthCode address ] )


main : Program Flags
main =
    Navigation.programWithFlags urlParser
        { init = init
        , update = update
        , urlUpdate = urlUpdate
        , subscriptions = (always Sub.none)
        , view = view
        }
