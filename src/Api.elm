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
import HttpBuilder
import Json.Decode as Decode
import Json.Decode exposing (Decoder)
import Json.Encode as Encode
import JsonApi
import JsonApi.Resources
import JsonApi.Documents
import JsonApi.Extra
import Api.Util exposing ((:>))
import Types exposing (..)
import Config


type Msg
    = GotAccessToken String
    | AuthFailed Http.Error
    | ProposalCreated Proposal
    | ProposalCreationFailed Http.Error
    | ProposalSupported Support
    | ProposalUnsupported String
    | SupportProposalFailed Http.Error
    | GotProposal Proposal
    | GettingProposalFailed Http.Error
    | GotProposalList ProposalList
    | GettingProposalListFailed Http.Error
    | GotMe Me



-- ENDPOINTS


tokenEndpoint : String
tokenEndpoint =
    Config.apiUrl ++ "/token"


meEndpoint : String
meEndpoint =
    Config.apiUrl ++ "/me"


newProposalEndpoint : String
newProposalEndpoint =
    Config.apiUrl ++ "/proposals"


supportProposalEndpoint : String
supportProposalEndpoint =
    Config.apiUrl ++ "/supports"


unsupportProposalEndpoint : String -> String
unsupportProposalEndpoint id =
    Config.apiUrl ++ "/proposals/" ++ id ++ "/support"


getProposalEndpoint : String -> String
getProposalEndpoint id =
    Config.apiUrl ++ "/proposals/" ++ id


getProposalListEndpoint : String
getProposalListEndpoint =
    Config.apiUrl ++ "/proposals"


facebookAuthUrl : String
facebookAuthUrl =
    let
        facebookRedirectUri =
            Config.baseRoot
                ++ "/"
                ++ Config.facebookRedirectPath
    in
        "https://www.facebook.com/dialog/oauth?client_id="
            ++ Config.facebookClientId
            ++ "&redirect_uri="
            ++ facebookRedirectUri



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
    Decode.map5 DecodedProposalAttributes
        (Decode.at [ "title" ] Decode.string)
        (Decode.at [ "body" ] Decode.string)
        (Decode.at [ "support-count" ] Decode.int)
        (Decode.at [ "authored-by-me" ] Decode.bool)
        (Decode.at [ "supported-by-me" ] Decode.bool)


decodeParticipantAttributes : Decoder String
decodeParticipantAttributes =
    Decode.at [ "name" ] Decode.string


encodeProposalInput : NewProposal -> Encode.Value
encodeProposalInput proposalInput =
    JsonApi.Extra.encodeDocument
        "proposal"
        Nothing
        [ ( "title", Encode.string proposalInput.title )
        , ( "body", Encode.string proposalInput.body )
        ]
        []


encodeSupportProposal : String -> Encode.Value
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


assembleSupport : JsonApi.Document -> Result String Support
assembleSupport document =
    JsonApi.Documents.primaryResource document
        :> \supportResource ->
            JsonApi.Resources.relatedResource "proposal" supportResource
                :> \proposalResource ->
                    JsonApi.Resources.attributes decodeProposalSupportAttributes proposalResource
                        :> \( supportCount, supportedByMe ) ->
                            Ok
                                { id = JsonApi.Resources.id supportResource
                                , proposal = JsonApi.Resources.id proposalResource
                                , supportCount = supportCount
                                , supportedByMe = supportedByMe
                                }


decodeProposalSupportAttributes : Decoder ( Int, Bool )
decodeProposalSupportAttributes =
    Decode.map2 (,)
        (Decode.at [ "support-count" ] Decode.int)
        (Decode.at [ "supported-by-me" ] Decode.bool)



-- COMMANDS


authenticate : String -> (Msg -> a) -> Cmd a
authenticate authCode wrapMsg =
    tokenEndpoint
        |> HttpBuilder.post
        |> HttpBuilder.withJsonBody
            (Encode.object [ ( "auth_code", Encode.string authCode ) ])
        |> HttpBuilder.withExpect (Http.expectJson decodeToken)
        |> HttpBuilder.toTask
        |> Api.Util.attempt AuthFailed GotAccessToken
        |> Cmd.map wrapMsg


getMe : String -> (Msg -> a) -> Cmd a
getMe accessToken wrapMsg =
    meEndpoint
        |> HttpBuilder.get
        |> Api.Util.withAccessToken accessToken
        |> Api.Util.withExpectJsonApi assembleMe
        |> HttpBuilder.toTask
        |> Api.Util.attempt AuthFailed GotMe
        |> Cmd.map wrapMsg


createProposal : NewProposal -> String -> (Msg -> a) -> Cmd a
createProposal proposalInput accessToken wrapMsg =
    newProposalEndpoint
        |> HttpBuilder.post
        |> Api.Util.withJsonApiBody (encodeProposalInput proposalInput)
        |> Api.Util.withAccessToken accessToken
        |> Api.Util.withExpectJsonApi assembleProposal
        |> HttpBuilder.toTask
        |> Api.Util.attempt ProposalCreationFailed ProposalCreated
        |> Cmd.map wrapMsg


supportProposal : String -> Bool -> String -> (Msg -> a) -> Cmd a
supportProposal id newState accessToken wrapMsg =
    if newState then
        supportProposalEndpoint
            |> HttpBuilder.post
            |> Api.Util.withJsonApiBody (encodeSupportProposal id)
            |> Api.Util.withAccessToken accessToken
            |> Api.Util.withExpectJsonApi assembleSupport
            |> HttpBuilder.toTask
            |> Api.Util.attempt SupportProposalFailed ProposalSupported
            |> Cmd.map wrapMsg
    else
        unsupportProposalEndpoint id
            |> HttpBuilder.delete
            |> Api.Util.withAccessToken accessToken
            |> HttpBuilder.toTask
            |> Api.Util.attempt SupportProposalFailed (\_ -> ProposalUnsupported id)
            |> Cmd.map wrapMsg


getProposal : String -> String -> (Msg -> a) -> Cmd a
getProposal id accessToken wrapMsg =
    getProposalEndpoint id
        |> HttpBuilder.get
        |> Api.Util.withAccessToken accessToken
        |> Api.Util.withExpectJsonApi assembleProposal
        |> HttpBuilder.toTask
        |> Api.Util.attempt GettingProposalFailed GotProposal
        |> Cmd.map wrapMsg


getProposalList : String -> (Msg -> a) -> Cmd a
getProposalList accessToken wrapMsg =
    getProposalListEndpoint
        |> HttpBuilder.get
        |> Api.Util.withAccessToken accessToken
        |> Api.Util.withExpectJsonApi assembleProposalList
        |> HttpBuilder.toTask
        |> Api.Util.attempt GettingProposalListFailed GotProposalList
        |> Cmd.map wrapMsg
