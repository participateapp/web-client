module Api
    exposing
        ( facebookAuthUrl
        , Msg(..)
        , authenticate
        , getMe
        , createProposal
        , getProposal
        )

import Http
import Task exposing (Task)
import Json.Decode as Decode
import Json.Decode exposing (Decoder)
import Json.Encode as Encode
import JsonApi
import JsonApi.Resources
import JsonApi.Documents
import JsonApi.Extra
import Types exposing (..)
import Api.Util exposing ((:>))


type Msg
    = GotAccessToken String
    | AuthFailed Http.Error
    | ProposalCreated Proposal
    | ProposalCreationFailed Http.Error
    | GotProposal Proposal
    | GettingProposalFailed Http.Error
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


getProposalEndpoint : String -> String
getProposalEndpoint id =
    apiUrl ++ "/proposals/" ++ id



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
        :> \proposalResource ->
            JsonApi.Resources.attributes decodeProposalAttributes proposalResource
                :> \( title, body ) ->
                    JsonApi.Resources.relatedResource "author" proposalResource
                        :> \participantResource ->
                            JsonApi.Resources.attributes decodeParticipantAttributes participantResource
                                :> \name ->
                                    Ok
                                        { id = JsonApi.Resources.id proposalResource
                                        , title = title
                                        , body = body
                                        , author =
                                            { id = JsonApi.Resources.id participantResource
                                            , name = name
                                            }
                                        }


decodeProposalAttributes : Decoder ( String, String )
decodeProposalAttributes =
    Decode.object2 (,)
        (Decode.at [ "title" ] Decode.string)
        (Decode.at [ "body" ] Decode.string)


decodeParticipantAttributes : Decoder String
decodeParticipantAttributes =
    Decode.at [ "name" ] Decode.string


encodeProposalInput : ProposalInput -> String
encodeProposalInput proposalInput =
    Encode.object
        [ ( "data"
          , Encode.object
                [ ( "type", Encode.string "proposal" )
                , ( "attributes"
                  , Encode.object
                        [ ( "title", Encode.string proposalInput.title )
                        , ( "body", Encode.string proposalInput.body )
                        ]
                  )
                ]
          )
        ]
        |> Encode.encode 0



-- COMMANDS


authenticate : String -> (Msg -> a) -> Cmd a
authenticate authCode wrapMsg =
    "{\"auth_code\": \""
        ++ authCode
        ++ "\"}"
        |> Api.Util.requestPost tokenEndpoint
        |> JsonApi.Extra.withHeader "Content-Type" "application/json"
        |> Http.send Http.defaultSettings
        |> Http.fromJson decodeToken
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


createProposal : ProposalInput -> String -> (Msg -> a) -> Cmd a
createProposal proposalInput accessToken wrapMsg =
    encodeProposalInput proposalInput
        |> Api.Util.requestPost newProposalEndpoint
        |> Api.Util.withAccessToken accessToken
        |> Api.Util.sendDefJsonApi assembleProposal
        |> Task.perform ProposalCreationFailed ProposalCreated
        |> Cmd.map wrapMsg


getProposal : String -> String -> (Msg -> a) -> Cmd a
getProposal id accessToken wrapMsg =
    getProposalEndpoint id
        |> Api.Util.requestGet
        |> Api.Util.withAccessToken accessToken
        |> Api.Util.sendDefJsonApi assembleProposal
        |> Task.perform GettingProposalFailed GotProposal
        |> Cmd.map wrapMsg
