module Component.NewProposal exposing (Model, init, Msg, update, view)

import Html exposing (..)
import Material
import Material.Button as Button
import Material.Textfield as Textfield
import Material.Color as Color
import Material.Grid exposing (grid, size, cell, Device(..))
import Form exposing (Form)
import Form.Field
import Form.Error
import Form.Validate exposing (Validation, form1, form2, get, string)
import Types exposing (..)


-- MODEL


type alias Model =
    { form : Form () NewProposal
    , mdl : Material.Model
    }


init : Model
init =
    { form = Form.initial [] validate
    , mdl = Material.model
    }


validate : Validation () NewProposal
validate =
    form2 NewProposal
        (get "title" string)
        (get "body" string)


type Msg
    = FormMsg Form.Msg
    | Mdl (Material.Msg Msg)



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg, Maybe NewProposal )
update msg model =
    case msg of
        FormMsg formMsg ->
            case ( formMsg, Form.getOutput model.form ) of
                ( Form.Submit, Just proposalInput ) ->
                    ( model
                    , Cmd.none
                      --! [ Api.createProposal proposalInput model.accessToken ApiMsg ]
                    )
                        |> addReturn (Just proposalInput)

                _ ->
                    ( { model | form = Form.update formMsg model.form }
                    , Cmd.none
                    )
                        |> addReturn Nothing

        Mdl msg' ->
            Material.update msg' model
                |> addReturn Nothing


addReturn : c -> ( a, b ) -> ( a, b, c )
addReturn c ( a, b ) =
    ( a, b, c )



-- VIEW


type alias Mdl =
    Material.Model


view : Model -> Html Msg
view model =
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
