module Api
    exposing
        ( facebookAuthUrl
        , Msg(..)
        , authenticate
        , getMe
        , createProposal
        , supportProposal
        , getProposal
        , getProposalList
        )

import Result.Extra
import Http
import Task exposing (Task)
import Json.Decode as Decode
import Json.Decode exposing (Decoder)
import Json.Encode as Encode
import JsonApi
import JsonApi.Resources
import JsonApi.Documents
import JsonApi.Extra
import Api.Util exposing ((:>))
import Types exposing (..)


type Msg
    = GotAccessToken String
    | AuthFailed Http.Error
    | ProposalCreated Proposal
    | ProposalCreationFailed Http.Error
    | ProposalSupported String
    | SupportProposalFailed Http.Error
    | GotProposal Proposal
    | GettingProposalFailed Http.Error
    | GotProposalList ProposalList
    | GettingProposalListFailed Http.Error
    | GotMe Me



-- ENDPOINTS


apiUrl : String
apiUrl =
    "http://localhost:4000"


tokenEndpoint : String
tokenEndpoint =
    apiUrl ++ "/token"


meEndpoint : String
meEndpoint =
    apiUrl ++ "/me"


newProposalEndpoint : String
newProposalEndpoint =
    apiUrl ++ "/proposals"


supportProposalEndpoint : String
supportProposalEndpoint =
    apiUrl ++ "/supports"


getProposalEndpoint : String -> String
getProposalEndpoint id =
    apiUrl ++ "/proposals/" ++ id


getProposalListEndpoint : String
getProposalListEndpoint =
    apiUrl ++ "/proposals"



-- TODO: move client_id and redirect_uri into environment variables


facebookAuthUrl : String
facebookAuthUrl =
    "https://www.facebook.com/dialog/oauth?client_id=1583083701926004&redirect_uri=http://localhost:3000/facebook_redirect"



-- DECODERS & ENCODERS


decodeToken : Decoder String
decodeToken =
    Decode.at [ "access_token" ] Decode.string


assembleMe : JsonApi.Document -> Result String Me
assembleMe document =
    JsonApi.Documents.primaryResource document
        :> \meResource ->
            JsonApi.Resources.attributes decodeMeAttributes meResource
                :> \name ->
                    Ok { name = name }


decodeMeAttributes : Decoder String
decodeMeAttributes =
    Decode.at [ "name" ] Decode.string


assembleProposal : JsonApi.Document -> Result String Proposal
assembleProposal document =
    JsonApi.Documents.primaryResource document
        :> assembleProposalFromResource


assembleProposalList : JsonApi.Document -> Result String ProposalList
assembleProposalList document =
    JsonApi.Documents.primaryResourceCollection document
        :> \proposalResourceList ->
            List.map
                assembleProposalFromResource
                proposalResourceList
                |> Result.Extra.combine


assembleProposalFromResource : JsonApi.Resource -> Result String Proposal
assembleProposalFromResource proposalResource =
    JsonApi.Resources.attributes decodeProposalAttributes proposalResource
        :> \proposalAttrs ->
            JsonApi.Resources.relatedResource "author" proposalResource
                :> \participantResource ->
                    JsonApi.Resources.attributes decodeParticipantAttributes participantResource
                        :> \name ->
                            Ok
                                { id = JsonApi.Resources.id proposalResource
                                , title = proposalAttrs.title
                                , body = proposalAttrs.body
                                , author =
                                    { id = JsonApi.Resources.id participantResource
                                    , name = name
                                    }
                                , supportCount = proposalAttrs.supportCount
                                , authoredByMe = proposalAttrs.authoredByMe
                                , supportedByMe = proposalAttrs.supportedByMe
                                }


type alias DecodedProposalAttributes =
    { title : String
    , body : String
    , supportCount : Int
    , authoredByMe : Bool
    , supportedByMe : Bool
    }


decodeProposalAttributes : Decoder DecodedProposalAttributes
decodeProposalAttributes =
    Decode.object5 DecodedProposalAttributes
        (Decode.at [ "title" ] Decode.string)
        (Decode.at [ "body" ] Decode.string)
        (Decode.at [ "support-count" ] Decode.int)
        (Decode.at [ "authored-by-me" ] Decode.bool)
        (Decode.at [ "supported-by-me" ] Decode.bool)


decodeParticipantAttributes : Decoder String
decodeParticipantAttributes =
    Decode.at [ "name" ] Decode.string


encodeProposalInput : NewProposal -> String
encodeProposalInput proposalInput =
    JsonApi.Extra.encodeDocument
        "proposal"
        Nothing
        [ ( "title", Encode.string proposalInput.title )
        , ( "body", Encode.string proposalInput.body )
        ]
        []


encodeSupportProposal : String -> String
encodeSupportProposal id =
    JsonApi.Extra.encodeDocument
        "support"
        Nothing
        []
        [ ( "proposal"
          , JsonApi.Extra.resourceLinkage <|
                Just ( "proposal", id )
          )
        ]



-- COMMANDS


authenticate : String -> (Msg -> a) -> Cmd a
authenticate authCode wrapMsg =
    ("{\"auth_code\": \"" ++ authCode ++ "\"}")
        |> Api.Util.requestPost tokenEndpoint
        |> JsonApi.Extra.withHeader "Content-Type" "application/json"
        |> Api.Util.sendDefJson decodeToken
        |> Task.perform AuthFailed GotAccessToken
        |> Cmd.map wrapMsg


getMe : String -> (Msg -> a) -> Cmd a
getMe accessToken wrapMsg =
    meEndpoint
        |> Api.Util.requestGet
        |> Api.Util.withAccessToken accessToken
        |> Api.Util.sendDefJsonApi assembleMe
        |> Task.perform AuthFailed GotMe
        |> Cmd.map wrapMsg


createProposal : NewProposal -> String -> (Msg -> a) -> Cmd a
createProposal proposalInput accessToken wrapMsg =
    encodeProposalInput proposalInput
        |> Api.Util.requestPost newProposalEndpoint
        |> Api.Util.withAccessToken accessToken
        |> Api.Util.sendDefJsonApi assembleProposal
        |> Task.perform ProposalCreationFailed ProposalCreated
        |> Cmd.map wrapMsg


supportProposal : String -> Bool -> String -> (Msg -> a) -> Cmd a
supportProposal id newState accessToken wrapMsg =
    -- ToDo: Send DELETE request to remove support (if newState == False)
    encodeSupportProposal id
        |> Api.Util.requestPost supportProposalEndpoint
        |> Api.Util.withAccessToken accessToken
        |> Api.Util.sendDefJsonApi (always (Ok ()))
        |> Task.perform SupportProposalFailed (always (ProposalSupported id))
        |> Cmd.map wrapMsg


getProposal : String -> String -> (Msg -> a) -> Cmd a
getProposal id accessToken wrapMsg =
    getProposalEndpoint id
        |> Api.Util.requestGet
        |> Api.Util.withAccessToken accessToken
        |> Api.Util.sendDefJsonApi assembleProposal
        |> Task.perform GettingProposalFailed GotProposal
        |> Cmd.map wrapMsg


getProposalList : String -> (Msg -> a) -> Cmd a
getProposalList accessToken wrapMsg =
    getProposalListEndpoint
        |> Api.Util.requestGet
        |> Api.Util.withAccessToken accessToken
        |> Api.Util.sendDefJsonApi assembleProposalList
        |> Task.perform GettingProposalListFailed GotProposalList
        |> Cmd.map wrapMsg
