module View exposing (..)

import Types exposing (..)
import Api
import String
import Html.App as App
import Html exposing (Html, text, h2, h3, h4, div, a)
import Html.Attributes exposing (style, href, class)
import Form exposing (Form)
import Form.Error
import Form.Field
import Material.Scheme
import Material.Color as Color
import Material.Layout as Layout
import Material.Grid exposing (grid, size, cell, Device(..))
import Material.Textfield as Textfield


view : Model -> Html Msg
view model =
  Material.Scheme.topWithScheme Color.Amber Color.Red <|
    Layout.render Mdl
      model.mdl
      [ Layout.fixedHeader
      ]
      { header =
        [ h4
          [ style [ ( "padding", ".5rem" ) ] ]
          [ text "Participate!" ]
        ]
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
          [ a
            [ href Api.facebookAuthUrl ]
            [ text "Login with Facebook" ]
          ]
      else
        div []
          [ text <| "Hello, " ++ (.name model.me)
          , h3 []
            [ a [ href "/new-proposal" ] [ text "Create a proposal" ] ]
          ]

    NewProposalRoute ->
      div []
        [ h2
          []
          [ text <| "New Proposal" ]
        , App.map FormMsg (formView model.form)
        ]

    NotFoundRoute ->
      div []
        [ text <| "Not found" ]

    FacebookRedirect ->
      div []
        [ text <| "Authenticating, please wait..." ]


formView : Form () Proposal -> Html Form.Msg
formView form =
  let
    errorFor field =
      case field.liveError of
        Just error ->
          div [ class "error" ] [ text (toString error) ]

        Nothing ->
          text ""

    title =
      Form.getFieldAsString "title" form

    body =
      Form.getFieldAsString "body" form
  in
    grid
      []
-- [ cell [ size All 12 ] [ titleField model ]
-- , cell [ size All 12 ] [ bodyField model ]
-- , cell [ size All 12 ] [ submitButton model ]
-- ]
      [ cell [ size All 12 ] [ text "title" ]
      , cell [ size All 12 ] [ text "body" ]
      , cell [ size All 12 ] [ text "submit?" ]
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
      [ 1, 0 ]
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
