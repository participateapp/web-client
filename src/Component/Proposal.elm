module Component.Proposal exposing (view)

import Html exposing (..)
import Html.Attributes exposing (disabled)
import Html.Events exposing (onClick)
import Types exposing (..)


view : (String -> Bool -> msg) -> Maybe Proposal -> String -> Html msg
view supportProposalMsg maybeProposal id =
    case maybeProposal of
        Nothing ->
            div [] [ text "Unknown proposal id: ", text id ]

        Just proposal ->
            div []
                [ div [] [ text "Title: ", text proposal.title ]
                , div [] [ text "Author: ", text proposal.author.name ]
                , div [] [ text "Body: ", text proposal.body ]
                , div [] [ text "Support Count: ", text <| toString proposal.supportCount ]
                , if proposal.authoredByMe then
                    button [ disabled True ]
                        [ text "Authored by me, automatically supported"
                        ]
                  else
                    button [ onClick <| supportProposalMsg id (not proposal.supportedByMe) ]
                        [ text <|
                            if proposal.supportedByMe then
                                "Unsupport this proposal (unimplemented)"
                            else
                                "Support this proposal"
                        ]
                ]
