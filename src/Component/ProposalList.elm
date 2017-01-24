module Component.ProposalList exposing (view)

import Html exposing (..)
import Html.Events exposing (onClick)
import Types exposing (..)


view : (String -> msg) -> ProposalList -> Html msg
view navigateMsg proposalList =
    ul [] <|
        List.map (viewEntry navigateMsg) proposalList


viewEntry : (String -> msg) -> Proposal -> Html msg
viewEntry navigateMsg proposal =
    li []
        [ i [] [ text proposal.author.name ]
        , text ": "
        , a
            [ onClick <| navigateMsg proposal.id ]
            [ text proposal.title ]
        ]
