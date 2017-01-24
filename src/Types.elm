module Types exposing (..)


type Route
    = Home
    | NewProposalRoute
    | ProposalRoute String
    | FacebookRedirect
    | NotFoundRoute


type alias Me =
    { name : String
    }


type alias NewProposal =
    { title : String
    , body : String
    }


type alias Proposal =
    { id : String
    , title : String
    , body : String
    , author : Participant
    , authoredByMe : Bool
    , supportCount : Int
    , supportedByMe : Bool
    }


type alias ProposalList =
    List Proposal


type alias Participant =
    { id : String
    , name : String
    }


type alias Support =
    { id : String
    , proposal : String
    , supportCount : Int
    , supportedByMe : Bool
    }
