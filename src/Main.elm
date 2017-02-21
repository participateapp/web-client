port module Main exposing (main)

import Html exposing (..)
import Html.App as App
import Html.Events exposing (..)
import Html.Attributes exposing (style, href, class, disabled, id)
import Material
import Material.Scheme
import Material.Button as Button
import Material.Textfield as Textfield
import Material.List as List
import Material.Options as Options exposing (css)
import Material.Layout as Layout
import Material.Color as Color
import Material.Menu as Menu
import Material.Elevation as Elevation
import Material.Grid as Grid exposing (grid, size, cell, Device(..))
import Material.Typography as Typography
import Material.Icon as Icon
import Material.Footer as Footer
import Material.Card as Card
import Material.Chip as Chip
import Material.Snackbar as Snackbar
import Material.Progress as Progress
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
import Http
import Hop
import Hop.Types exposing (Config, Address, Query)
import Types exposing (..)
import Config exposing (basePath)
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
    { basePath = basePath
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

        model1ps =
            model1 |> progressStart

        _ =
            Debug.log "urlUpdate" ( route, address )
    in
        if String.isEmpty model.accessToken && route /= Home then
            ( model1ps, Navigation.newUrl <| Hop.outputFromPath hopConfig "/" )
        else
            case route of
                ProposalRoute id ->
                    case Dict.get id model.proposals of
                        Nothing ->
                            ( model1ps
                            , Api.getProposal id model.accessToken ApiMsg
                            )

                        Just _ ->
                            ( model1, Cmd.none )

                Home ->
                    ( model1ps
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
    , snackbar : Snackbar.Model ()
    , progress : Bool
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
    , snackbar = Snackbar.model
    , progress = False
    }



-- UPDATE


type Msg
    = ApiMsg Api.Msg
    | NavigateToPath String
    | FormMsg Form.Msg
    | NoOp
    | Mdl (Material.Msg Msg)
    | SnackbarMsg (Snackbar.Msg ())
    | SupportProposal String Bool


addProposal : Proposal -> Model -> Model
addProposal proposal model =
    { model | proposals = Dict.insert proposal.id proposal model.proposals }


addProposalList : ProposalList -> Model -> Model
addProposalList proposalList model =
    List.foldl addProposal model proposalList


withSnackbarNote : String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
withSnackbarNote snackContent ( model, cmd ) =
    let
        ( snackModel, snackCmd ) =
            Snackbar.add
                (Snackbar.toast () snackContent)
                model.snackbar
    in
        ( { model | snackbar = snackModel }
        , Cmd.batch [ cmd, Cmd.map SnackbarMsg snackCmd ]
        )


withHttpErrorResponse : String -> Http.Error -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
withHttpErrorResponse contextText httpError ( model, cmd ) =
    withSnackbarNote
        (contextText ++ ": " ++ toString httpError)
        ( { model | error = Just <| toString httpError } |> progressDone
        , Cmd.none
        )


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


progressStart : Model -> Model
progressStart model =
    { model | progress = True }


progressDone : Model -> Model
progressDone model =
    { model | progress = False }


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
                    withHttpErrorResponse
                        "Authentication failed"
                        httpError
                        ( model, Cmd.none )

                Api.GotMe me ->
                    ( { model | me = me } |> progressDone
                    , Navigation.newUrl <| Hop.outputFromPath hopConfig "/"
                    )

                Api.ProposalCreated proposal ->
                    ( model
                        |> addProposal proposal
                    , Navigation.newUrl <|
                        Hop.output hopConfig { path = [ "proposals", proposal.id ], query = Dict.empty }
                    )
                        |> withSnackbarNote "Proposal saved"

                Api.ProposalCreationFailed httpError ->
                    withHttpErrorResponse
                        "Saving proposal failed"
                        httpError
                        ( model, Cmd.none )

                Api.ProposalSupported support ->
                    ( model |> updateProposalSupport support |> progressDone
                    , Cmd.none
                    )
                        |> withSnackbarNote "Proposal supported"

                Api.SupportProposalFailed httpError ->
                    withHttpErrorResponse
                        "Supporting proposal failed"
                        httpError
                        ( model, Cmd.none )

                Api.GotProposal proposal ->
                    ( model
                        |> addProposal proposal
                        |> progressDone
                    , Cmd.none
                    )

                Api.GettingProposalFailed httpError ->
                    withHttpErrorResponse
                        "Loading proposal failed"
                        httpError
                        ( model, Cmd.none )

                Api.GotProposalList proposalList ->
                    ( model
                        |> addProposalList proposalList
                        |> progressDone
                    , Cmd.none
                    )

                Api.GettingProposalListFailed httpError ->
                    withHttpErrorResponse
                        "Loading proposal list failed"
                        httpError
                        ( model, Cmd.none )

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

        SnackbarMsg snackMsg ->
            -- Snackbar currently has no builtin elm-mdl-component support.
            -- Have to wire up manually here.
            -- https://github.com/debois/elm-mdl/blob/6340ec3c83875c35a5a89e7bf4408c1ca1cbfdcb/src/Material/Snackbar.elm#L311-L328
            let
                ( snackModel, snackCmd ) =
                    Snackbar.update snackMsg model.snackbar
            in
                ( { model | snackbar = snackModel }
                , Cmd.map SnackbarMsg snackCmd
                )

        SupportProposal id newState ->
            ( model |> progressStart
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
    Layout.render Mdl
        model.mdl
        [ Layout.fixedHeader
        ]
        { header = viewHeader model
        , drawer = []
        , tabs = ( [], [] )
        , main = viewMain model
        }


viewHeader : Model -> List (Html Msg)
viewHeader model =
    [ Layout.row [ Color.background Color.white ] <|
        [ a
            [ href "/"
            , class "main-title"
            ]
            [ Layout.title [ Color.text <| Color.color Color.Cyan Color.S800 ]
                [ text "Participate!"
                ]
            ]
        , Layout.spacer
        ]
            ++ if String.isEmpty model.accessToken then
                [ viewLoginButton model ]
               else
                [ div [ class "mdl-layout--large-screen-only" ]
                    [ Button.render Mdl
                        [ 2 ]
                        model.mdl
                        [ Options.id "new-proposal"
                        , Button.colored
                        , Button.onClick <| NavigateToPath "/new-proposal"
                        ]
                        [ text "New proposal" ]
                    ]
                , viewUserNavigation model
                ]
    ]


viewLoginButton : Model -> Html Msg
viewLoginButton model =
    a [ href Api.facebookAuthUrl ]
        [ img
            [ Html.Attributes.src <| basePath ++ "/images/facebook-sign-in.png"
            , class "login-button-img"
            ]
            []
        ]


viewUserNavigation : Model -> Html Msg
viewUserNavigation model =
    let
        usernameColor =
            Color.text <| Color.color Color.Grey Color.S700
    in
        Layout.navigation []
            -- According to the mockup, the button should display user's avatar.
            -- But elm-mdl currently only supports icons for menu buttons.
            -- See: https://github.com/debois/elm-mdl/issues/165
            [ Menu.render Mdl
                [ 0 ]
                model.mdl
                [ Menu.ripple
                , Menu.bottomRight
                , Color.text <| Color.primary
                ]
                [ Menu.item
                    [ Menu.disabled, usernameColor ]
                    [ text "Signed in as" ]
                , Menu.item
                    [ Menu.divider, Menu.disabled, usernameColor ]
                    [ strong [] [ text model.me.name ] ]
                , Menu.item
                    [ Menu.onSelect <| NavigateToPath "/new-proposal" ]
                    [ text "New proposal" ]
                , Menu.item
                    [ Menu.onSelect NoOp ]
                    [ text "Sign out" ]
                ]
            ]


viewMain : Model -> List (Html Msg)
viewMain model =
    [ if model.progress then
        Progress.indeterminate
      else
        Progress.progress 0.0
    , case model.route of
        Home ->
            if String.isEmpty model.accessToken then
                viewLandingPage model
            else
                viewProposalList model

        NewProposalRoute ->
            viewNewProposal model

        ProposalRoute id ->
            div [] [ viewProposal model id ]

        NotFoundRoute ->
            div []
                [ text <| "Not found" ]

        FacebookRedirect ->
            div []
                [ text <| "Authenticating, please wait..." ]
    , viewFooter model
    , Snackbar.view model.snackbar |> App.map SnackbarMsg
    ]


viewLandingPage : Model -> Html Msg
viewLandingPage model =
    div [ id "landing-pg" ]
        [ section [ id "hero" ]
            [ grid [ Options.cs "content-grid" ]
                [ cell [ size All 6, Typography.center ]
                    [ viewLoginButton model ]
                , cell [ size All 6 ]
                    [ Options.styled h1
                        [ Typography.display1
                        , Typography.contrast 1
                        , Color.text <| Color.color Color.Cyan Color.S800
                        ]
                        [ text "Participate!" ]
                    , Options.styled p
                        [ Typography.headline, Color.text <| Color.color Color.Cyan Color.S800 ]
                        [ text "An App for Democratic Decision Making" ]
                    ]
                ]
            ]
        , let
            feature icon text_ =
                cell [ size All 4, Typography.center ]
                    [ p [] [ Icon.i icon ]
                    , p [] [ text text_ ]
                    ]
          in
            section [ id "main-top" ]
                [ grid [ Options.cs "content-grid" ]
                    [ feature "assignment"
                        "A participant makes a proposal and gathers support for it. Other participants can collaborate on it if they support it in principle."
                    , feature "announcement"
                        "Dissenters have to make a counter-proposal, and gather support for it as well, to be heard."
                    , feature "call_merge"
                        "Representation is ensured for participants who are less involved through fluid delegation of support, in a liquid democracy."
                    ]
                ]
        , let
            feature turn title txt =
                grid [ Options.cs "content-grid" ] <|
                    (if turn then
                        List.reverse
                     else
                        identity
                    )
                        [ cell [ size All 6 ]
                            [ Options.styled p [ Typography.title ] [ text title ]
                            , p [] [ text txt ]
                            ]
                        , cell [ size All 6 ]
                            [ Options.div
                                [ Elevation.e2, Options.css "height" "150px" ]
                                []
                            ]
                        ]
          in
            section [ id "main-middle" ]
                [ feature True
                    "Concrete Proposals"
                    "Participate! focuses on concrete proposals rather than noisy and many times unproductive debate."
                , feature False
                    "Ensured Representation"
                    "Representation is ensured for participants who are less involved (be it for lack of time, inclination or of knowledge) through fluid delegation of support, in a liquid democracy."
                ]
        , Options.styled' section
            [ Color.background <| Color.color Color.Grey Color.S200 ]
            [ id "main-lower" ]
            [ grid [ Options.cs "content-grid" ]
                [ cell [ size All 12, Typography.center ]
                    [ Options.styled h2
                        [ Typography.headline ]
                        [ text "Want to get involved?" ]
                    , p []
                        [ strong []
                            [ text "We pair program so you can get up to speed quickly and help us develop features"
                            ]
                        ]
                    , p []
                        [ a [ href "mailto:oliverbwork@gmail.com" ]
                            [ text "Shoot us an email"
                            ]
                        , text ", we'll add you to our Slack channel to join the discussion and talk about next steps."
                        ]
                    , p []
                        [ text "See the complete guide to contributing "
                        , a
                            [ href "https://github.com/participateapp/web-client/blob/master/CONTRIBUTING.md"
                            , Html.Attributes.target "_blank"
                            ]
                            [ text "here"
                            ]
                        , text "."
                        ]
                    ]
                ]
            ]
        ]


viewFooter : Model -> Html Msg
viewFooter model =
    Footer.mega []
        { top =
            Footer.top []
                { left =
                    Footer.left []
                        {-
                           -- elm-mdl has special functions for the footer contents, which we don't use here, because:
                           -- Class mdl-mega-footer__link-list puts the items to-to-down. We want them left-to-right.
                           -- Also, don't know how Footer.socialButton is supposed to work.
                           [ Footer.links [ Options.cs "social-links" ]
                               [ Footer.linkItem [ Footer.href "https://github.com/participateapp/web-client" ]
                                   [ Footer.html <|
                                       img [ Html.Attributes.src <| basePath ++ "/images/github-circle.png" ] []
                                   ]
                               , Footer.linkItem [ Footer.href "https://github.com/participateapp/web-client" ]
                                   [ Footer.html <|
                                       img [ Html.Attributes.src <| basePath ++ "/images/github-circle.png" ] []
                                   ]
                               ]
                           ]
                        -}
                        [ Footer.html <|
                            ul [ class "mdl-mini-footer__link-list social-links" ]
                                [ li []
                                    [ a [ Html.Attributes.href "https://github.com/participateapp/web-client" ]
                                        [ img [ Html.Attributes.src <| basePath ++ "/images/github-circle.png" ] [] ]
                                    ]
                                , li []
                                    [ a [ Html.Attributes.href "https://participateapp.slack.com" ]
                                        [ img [ Html.Attributes.src <| basePath ++ "/images/slack.png" ] [] ]
                                    ]
                                , li []
                                    [ a [ Html.Attributes.href "https://twitter.com/digiberber" ]
                                        [ img [ Html.Attributes.src <| basePath ++ "/images/twitter.png" ] [] ]
                                    ]
                                ]
                        ]
                , right =
                    Footer.right []
                        [ Footer.html <| Options.styled p [ Typography.title ] [ text "Participate!" ]
                        , Footer.html <| p [] [ text "An open source liquid democracy application." ]
                        ]
                }
        , middle = Nothing
        , bottom =
            Footer.bottom []
                [ Footer.html <|
                    ul [ class "mdl-mini-footer__link-list" ]
                        [ li []
                            [ a [ Html.Attributes.href "https://github.com/participateapp/web-client/blob/master/CONTRIBUTING.md" ]
                                [ text "Guide to contributing" ]
                            ]
                        , li []
                            [ a [ Html.Attributes.href "https://github.com/participateapp/web-client/wiki/Development-Setup" ]
                                [ text "Wiki" ]
                            ]
                        ]
                ]
        }


viewNewProposal : Model -> Html Msg
viewNewProposal model =
    Options.styled div
        [ Color.background <| Color.color Color.Grey Color.S200 ]
        [ div [ class "content-col narrow new-proposal-col" ]
            [ Card.view
                [ Options.cs "new-proposal"
                , Color.background <| Color.white
                ]
                [ Card.text []
                    [ titleField model
                    , bodyField model
                    ]
                , Card.text
                    [ Options.cs "mdl-grid" ]
                    [ Layout.spacer
                    , Button.render Mdl
                        [ 5 ]
                        model.mdl
                        [ Button.raised
                        , Button.ripple
                        , Button.colored
                        , Button.onClick <| FormMsg <| Form.Submit
                        ]
                        [ text "Save" ]
                    , Layout.spacer
                    ]
                ]
            ]
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
             , Textfield.rows 6
             , Textfield.value <| Maybe.withDefault "" body.value
             , Textfield.onInput <| FormMsg << (Form.Field.Text >> Form.Input body.path)
             , Textfield.onFocus <| FormMsg <| Form.Focus body.path
             , Textfield.onBlur <| FormMsg <| Form.Blur body.path
             ]
                ++ conditionalProperties
            )


viewProposal : Model -> String -> Html Msg
viewProposal model id =
    let
        cardContent =
            case Dict.get id model.proposals of
                Nothing ->
                    [ Card.title [] [ text "Unknown proposal id: ", text id ] ]

                Just proposal ->
                    [ Card.actions [ Options.cs "mdl-grid border-bottom" ]
                        [ span [ class "actions__authored" ]
                            [ img
                                [ class "mdl-chip__contact"
                                , Html.Attributes.src <| basePath ++ "/images/john.jpg"
                                ]
                                []
                            , span [ class "authored" ]
                                [ text proposal.author.name
                                , br [] []
                                , text "2 days ago"
                                ]
                            ]
                        , Layout.spacer
                        , Chip.span []
                            [ Chip.content []
                                [ text "Support count: "
                                , text <| toString proposal.supportCount
                                ]
                            ]
                        , Layout.spacer
                        , if proposal.authoredByMe then
                            Button.render Mdl
                                [ 3 ]
                                model.mdl
                                [ Options.id "support-proposal"
                                , Button.colored
                                , Button.accent
                                , Button.disabled
                                ]
                                [ text "Authored by me" ]
                          else
                            Button.render Mdl
                                [ 3 ]
                                model.mdl
                                ([ Options.id "support-proposal"
                                 , Button.colored
                                 , Button.onClick <| SupportProposal id (not proposal.supportedByMe)
                                 ]
                                    ++ if proposal.supportedByMe then
                                        [ Color.text <| Color.color Color.Green Color.S500 ]
                                       else
                                        [ Button.raised
                                        , Button.accent
                                        ]
                                )
                                [ text <|
                                    if proposal.supportedByMe then
                                        "Supporting"
                                    else
                                        "Support Proposal"
                                ]
                        ]
                    , Card.title []
                        [ Card.head
                            [ Typography.headline
                            , Color.text <| Color.color Color.Grey Color.S700
                            ]
                            [ text proposal.title ]
                        , Card.subhead
                            [ Typography.subhead ]
                            [ strong []
                                [ text "Here goes the summary, which is yet to be implemented ..." ]
                            ]
                        ]
                    , Card.text []
                        [ text proposal.body ]
                    ]
    in
        Options.styled div
            [ Color.background <| Color.color Color.Grey Color.S200 ]
            [ div [ class "content-col narrow proposal-col" ]
                [ Card.view
                    [ Options.cs "proposal-show"
                    , Color.background <| Color.white
                    ]
                    cardContent
                ]
            ]


viewProposalList : Model -> Html Msg
viewProposalList model =
    Options.styled div
        [ Color.background <| Color.color Color.Grey Color.S200 ]
        [ div [ class "content-col narrow proposal-list-col" ] <|
            List.map viewProposalListEntry (Dict.values model.proposals)
        ]


viewProposalListEntry : Proposal -> Html Msg
viewProposalListEntry proposal =
    Card.view
        [ Options.attribute <| onClick <| NavigateToPath <| "proposals/" ++ proposal.id
        , Color.background <| Color.white
        ]
        [ Card.title []
            [ div [ class "proposal-card-title" ]
                [ Card.head
                    [ Options.cs "proposal-title"
                    , Color.text <| Color.primary
                    ]
                    [ text proposal.title ]
                , div [ class "proposal-state" ]
                    [ div []
                        [ Chip.span
                            [ Typography.center
                            ]
                            [ Chip.text [] <| toString proposal.supportCount ]
                        ]
                    , if proposal.authoredByMe || proposal.supportedByMe then
                        Options.styled
                            span
                            [ Color.text <| Color.color Color.Green Color.S500 ]
                            [ text <|
                                if proposal.authoredByMe then
                                    "My Proposal"
                                else
                                    "Supporting"
                            ]
                      else
                        text ""
                    ]
                ]
            ]
        , Card.text [] [ text proposal.body ]
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
        ( model1
        , Cmd.batch
            [ cmd1
            , checkForAuthCode address
            , Layout.sub0 Mdl
            ]
        )


main : Program Flags
main =
    Navigation.programWithFlags urlParser
        { init = init
        , update = update
        , urlUpdate = urlUpdate
        , subscriptions =
            \model ->
                Sub.batch
                    [ Layout.subs Mdl model.mdl
                    , Menu.subs Mdl model.mdl
                    ]
        , view = view
        }
