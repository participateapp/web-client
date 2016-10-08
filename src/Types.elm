module Types exposing (..)


import Hop.Types exposing (Config, Address, Query)
import Form exposing (Form)
import Material
import Http


type Route
  = Home
  | NewProposalRoute
  | FacebookRedirect
  | NotFoundRoute


type alias Proposal =
  { title : String
  , body : String
  }


type alias Model =
  { route : Route
  , address: Address
  , accessToken: String
  , error : Maybe String
  , me : Me
  , form : Form () Proposal
  , mdl : Material.Model
  }


type alias Mdl =
  Material.Model


type Msg
  = ApiMsg AsyncActionMsg
  | FormMsg Form.Msg
  | NewProposalFormMsg
  | NoOp
  | Mdl (Material.Msg Msg)


type AsyncActionMsg
  = GotAccessToken String
  | AuthFailed Http.Error
  | ProposalCreated Proposal
  | ProposalCreationFailed Http.Error
  | GotMe Me


type alias Me =
  { name : String
  }

