module Types exposing (..)


type alias Me =
    { name : String
    }


type alias ProposalAttr =
    { title : String
    , body : String
    }


type alias Proposal =
    { id : String
    , author : String
    , attr : ProposalAttr
    }


type alias ParticipantAttr =
    { name : String
    }


type alias Participant =
    { id : String
    , attr : ParticipantAttr
    }



{-
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
-}
