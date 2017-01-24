port module Main exposing (main)

import Html exposing (..)
import Html.App as App
import Html.Events exposing (..)
import Html.Attributes exposing (style, href, class, disabled)
import Material
import Material.Scheme
import Material.Layout as Layout
import Material.Color as Color
import Dict exposing (Dict)
import String
import Navigation
import UrlParser exposing ((</>))
import Hop
import Hop.Types exposing (Config, Address, Query)
import Types exposing (..)
import Api
import Component.NewProposal
import Component.ProposalList
import Component.Proposal


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
            { model | route = route }

        _ =
            Debug.log "urlUpdate" ( route, address )
    in
        if String.isEmpty model.accessToken && route /= Home then
            ( model1, Navigation.newUrl "/" )
        else
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
    , accessToken : String
    , error : Maybe String
    , me : Me
    , newProposal : Component.NewProposal.Model
    , mdl : Material.Model
    , proposals : Dict String Proposal
    }


initialModel : String -> Route -> Model
initialModel accessToken route =
    { route = route
    , accessToken = accessToken
    , error = Nothing
    , me = { name = "" }
    , newProposal = Component.NewProposal.init
    , mdl = Material.model
    , proposals = Dict.empty
    }



-- UPDATE


type Msg
    = ApiMsg Api.Msg
    | NavigateToPath String
    | NewProposalMsg Component.NewProposal.Msg
    | NoOp
    | Mdl (Material.Msg Msg)
    | SupportProposal String Bool


addProposal : Proposal -> Model -> Model
addProposal proposal model =
    { model | proposals = Dict.insert proposal.id proposal model.proposals }


addProposalList : ProposalList -> Model -> Model
addProposalList proposalList model =
    List.foldl addProposal model proposalList


updateProposalSupport : Support -> Model -> Model
updateProposalSupport support model =
    let
        newProposals =
            Dict.update support.proposal
                (Maybe.map
                    (\proposal ->
                        { proposal
                            | supportCount = support.supportCount
                            , supportedByMe = support.supportedByMe
                        }
                    )
                )
                model.proposals
    in
        { model | proposals = newProposals }


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
                    ( model |> addProposal proposal
                    , Navigation.newUrl <| "proposals/" ++ toString proposal.id
                    )

                Api.ProposalCreationFailed httpError ->
                    ( { model | error = Just <| toString httpError }, Cmd.none )

                Api.ProposalSupported support ->
                    ( model |> updateProposalSupport support
                    , Cmd.none
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
            , Navigation.newUrl path
            )

        NewProposalMsg newProposalMsg ->
            let
                ( compModel, compCmd, maybeNewProposal ) =
                    Component.NewProposal.update newProposalMsg model.newProposal

                apiCmd =
                    case maybeNewProposal of
                        Just newProposalInput ->
                            Api.createProposal newProposalInput model.accessToken ApiMsg

                        Nothing ->
                            Cmd.none
            in
                ( { model | newProposal = compModel }
                , Cmd.batch
                    [ Cmd.map NewProposalMsg compCmd
                    , apiCmd
                    ]
                )

        NoOp ->
            ( model, Cmd.none )

        Mdl msg' ->
            Material.update msg' model

        SupportProposal id newState ->
            ( model
            , Api.supportProposal id newState model.accessToken ApiMsg
            )



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
            if String.isEmpty model.accessToken then
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
                    , div []
                        [ Component.ProposalList.view
                            (\id -> NavigateToPath <| "proposals/" ++ id)
                            (Dict.values model.proposals)
                        ]
                    ]

        NewProposalRoute ->
            div []
                [ h2 []
                    [ text <| "New Proposal" ]
                , Component.NewProposal.view model.newProposal
                    |> App.map NewProposalMsg
                ]

        ProposalRoute id ->
            div []
                [ h2 [] [ text "Proposal" ]
                , Component.Proposal.view
                    SupportProposal
                    (Dict.get id model.proposals)
                    id
                ]

        NotFoundRoute ->
            div []
                [ text <| "Not found" ]

        FacebookRedirect ->
            div []
                [ text <| "Authenticating, please wait..." ]



-- APP


type alias Flags =
    { accessToken : Maybe String }


init : Flags -> ( Route, Address ) -> ( Model, Cmd Msg )
init flags ( route, address ) =
    let
        model0 =
            initialModel (Maybe.withDefault "" flags.accessToken) route

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
