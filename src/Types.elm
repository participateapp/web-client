module Types exposing (..)


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
    }


type alias Participant =
    { id : String
    , name : String
    }
