module Update exposing (update, validate)

import Types exposing (..)
import Api
import Navigation
import Form exposing (Form)
import Form.Validate exposing (Validation, form1, form2, get, string)
import Material


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ApiMsg apiMsg ->
            case apiMsg of
                GotAccessToken accessToken ->
                    ( { model | accessToken = accessToken }, Api.getMeCmd accessToken ApiMsg )

                AuthFailed httpError ->
                    ( { model | error = Just <| toString httpError }, Cmd.none )

                GotMe me ->
                    ( { model | me = me }, Navigation.newUrl "/" )

                ProposalCreated proposal ->
                    ( model, Navigation.newUrl "/" )

                ProposalCreationFailed httpError ->
                    ( { model | error = Just <| toString httpError }, Cmd.none )

        FormMsg formMsg ->
            case ( formMsg, Form.getOutput model.form ) of
                ( Form.Submit, Just proposal ) ->
                    model ! [ Api.createProposalCmd proposal model.accessToken ApiMsg ]

                _ ->
                    ( { model | form = Form.update formMsg model.form }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

        Mdl msg' ->
            Material.update msg' model

        NewProposalFormMsg ->
            ( model, Cmd.none )



-- ----------------------------------------------------------------- helpers


validate : Validation () Proposal
validate =
    form2 Proposal
        (get "title" string)
        (get "body" string)
