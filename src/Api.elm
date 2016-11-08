module Api
  exposing
    ( facebookAuthUrl
    , Msg(..), Me
    , authenticateCmd, getMeCmd, createProposalCmd, getProposalCmd
    , getProposalListCmd
    )


import Http
import Task exposing (Task)
import Json.Decode as Decode
import Json.Decode exposing (Decoder)
import Json.Encode as Encode


type Msg
  = GotAccessToken String
  | AuthFailed Http.Error
  | ProposalCreated Proposal Participant
  | ProposalCreationFailed Http.Error
  | GotProposal Proposal Participant
  | GettingProposalFailed Http.Error
  | GotProposalList ProposalList
  | GettingProposalListFailed Http.Error
  | GotMe Me


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
  , attr: ProposalAttr
  }


type alias ParticipantAttr =
  { name : String
  }


type alias Participant =
  { id : String
  , attr: ParticipantAttr
  }


type alias ProposalList =
  List Proposal



-- ENDPOINTS


apiUrl : String
apiUrl = "http://localhost:4000"


tokenEndpoint : String
tokenEndpoint = apiUrl ++ "/token"


meEndpoint : String
meEndpoint = apiUrl ++ "/me"


newProposalEndpoint : String
newProposalEndpoint = apiUrl ++ "/proposals"


getProposalEndpoint : String -> String
getProposalEndpoint id =
  apiUrl ++ "/proposals/" ++ id


proposalListEndpoint : String
proposalListEndpoint = apiUrl ++ "/proposals"


-- TODO: move client_id and redirect_uri into environment variables
facebookAuthUrl : String
facebookAuthUrl = "https://www.facebook.com/dialog/oauth?client_id=1583083701926004&redirect_uri=http://localhost:3000/facebook_redirect"



-- DECODERS & ENCODERS


decoderAssertType : List String -> String -> Decoder a -> Decoder a
decoderAssertType path expectedType decoder =
  Decode.at path Decode.string
    `Decode.andThen` \actualType ->
      if actualType == expectedType then
        decoder
      else
        Decode.fail <|
          "Expected type \"" ++ expectedType ++ "\", got \"" ++ actualType ++ "\""


decodeData : Decoder a -> Decoder a
decodeData =
  Decode.at ["data"]


decodeToken : Decoder String
decodeToken =
  Decode.at ["access_token"] Decode.string


decodeMe : Decoder Me
decodeMe =
  decodeData <|
    decoderAssertType ["type"] "participant" <|
      Decode.object1 Me
        (Decode.at ["attributes", "name"] Decode.string)


decodeProposal : Decoder Proposal
decodeProposal =
  decoderAssertType ["type"] "proposal" <|
    Decode.object3
      Proposal
      ( Decode.at ["id"] Decode.string )
      ( Decode.at ["relationships", "author", "data"] <|
          decoderAssertType ["type"] "participant" <|
            Decode.at ["id"] Decode.string
      )
      ( Decode.object2 ProposalAttr
          (Decode.at ["attributes", "title"] Decode.string)
          (Decode.at ["attributes", "body"] Decode.string)
      )


decodeProposalIncluded : Decoder (List Participant)
decodeProposalIncluded =
  Decode.at ["included"] <|
    Decode.list <|
      decoderAssertType ["type"] "participant" <|
        Decode.object2
          Participant
          ( Decode.at ["id"] Decode.string )
          ( Decode.object1 ParticipantAttr
              (Decode.at ["attributes", "name"] Decode.string)
          )


decodeProposalIncludedAuthor : Decoder Participant
decodeProposalIncludedAuthor =
  Decode.andThen
    decodeProposalIncluded
    ( \included ->
        case included of
          [author] -> Decode.succeed author
          _ -> Decode.fail "No author included in JSON for proposal"
    )


decodeProposalAndAuthor : Decoder (Proposal, Participant)
decodeProposalAndAuthor =
  Decode.object2
    (,)
    ( decodeData decodeProposal )
    decodeProposalIncludedAuthor

decodeProposalList : Decoder ProposalList
decodeProposalList =
  decodeData <|
    Decode.list decodeProposal


encodeProposal : ProposalAttr -> String
encodeProposal proposal =
  -- http://noredink.github.io/json-to-elm/
  Encode.object
    [ ( "data"
      , Encode.object
          [ ( "type", Encode.string "proposal" )
          , ( "attributes"
            , Encode.object
              [ ( "title", Encode.string proposal.title )
              , ( "body", Encode.string proposal.body )
              ]
            )
          ]
      )
    ] 
    |> Encode.encode 0



-- COMMANDS


authenticateCmd : String -> (Msg -> a) -> Cmd a
authenticateCmd authCode wrapMsg =
  let
    body =
      "{\"auth_code\": \"" ++ authCode ++ "\"}"
    
    requestTask =
      exchangeAuthCodeForToken body
  in
    Task.perform AuthFailed GotAccessToken requestTask
      |> Cmd.map wrapMsg


getMeCmd : String -> (Msg -> a) -> Cmd a
getMeCmd accessToken wrapMsg =
  getWithToken meEndpoint accessToken decodeMe
    |> Task.perform AuthFailed GotMe
    |> Cmd.map wrapMsg


createProposalCmd : ProposalAttr -> String -> (Msg -> a) -> Cmd a
createProposalCmd proposal accessToken wrapMsg =
  postProposal proposal accessToken
    |> Task.perform ProposalCreationFailed (uncurry ProposalCreated)
    |> Cmd.map wrapMsg


getProposalCmd : String -> String -> (Msg -> a) -> Cmd a
getProposalCmd id accessToken wrapMsg =
  getProposal id accessToken
    |> Task.perform GettingProposalFailed (uncurry GotProposal)
    |> Cmd.map wrapMsg


getProposalListCmd : String -> (Msg -> a) -> Cmd a
getProposalListCmd accessToken wrapMsg =
  getProposalList accessToken
    |> Task.perform GettingProposalListFailed GotProposalList
    |> Cmd.map wrapMsg



-- HTTP requests


exchangeAuthCodeForToken : String -> Task Http.Error String
exchangeAuthCodeForToken body =
  { verb = "POST", headers = [("Content-Type", "application/json")], url = tokenEndpoint, body = Http.string body }
    |> Http.send Http.defaultSettings
    |> Http.fromJson decodeToken


postProposal : ProposalAttr -> String -> Task Http.Error (Proposal, Participant)
postProposal proposal accessToken =
  { verb = "POST"
  , headers =
      [ ("Authorization", "Bearer " ++ accessToken)
      , ("Content-Type", "application/vnd.api+json")
      ]
  , url = newProposalEndpoint
  , body = Http.string (encodeProposal proposal)
  }
    |> Http.send Http.defaultSettings
    |> Http.fromJson decodeProposalAndAuthor


getProposal : String -> String -> Task Http.Error {- Maybe -} (Proposal, Participant)
getProposal id accessToken =
  { verb = "GET"
  , headers =
      [ ("Authorization", "Bearer " ++ accessToken)
      , ("Content-Type", "application/vnd.api+json")
      ]
  , url = getProposalEndpoint id
  , body = Http.empty
  }
    |> Http.send Http.defaultSettings
    |> Http.fromJson decodeProposalAndAuthor


getProposalList : String -> Task Http.Error ProposalList
getProposalList accessToken =
  { verb = "GET"
  , headers =
      [ ("Authorization", "Bearer " ++ accessToken)
      , ("Content-Type", "application/vnd.api+json")
      ]
  , url = proposalListEndpoint
  , body = Http.empty
  }
    |> Http.send Http.defaultSettings
    |> Http.fromJson decodeProposalList


getWithToken : String -> String -> Decoder a -> Task Http.Error a
getWithToken url accessToken responseDecoder  =
  { verb = "GET"
  , headers =
      [ ("Authorization", "Bearer " ++ accessToken)
      , ("Content-Type", "application/vnd.api+json")
      ]
  , url = url
  , body = Http.empty
  }
  |> Http.send Http.defaultSettings
  |> Http.fromJson responseDecoder
