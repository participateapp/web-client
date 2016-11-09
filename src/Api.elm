module Api
    exposing
        ( facebookAuthUrl
        , Msg(..)
        , authenticateCmd
        , getMeCmd
        , createProposalCmd
        , getProposalCmd
        )

import Http
import Task exposing (Task)
import Json.Decode as Decode
import Json.Decode exposing (Decoder)
import Json.Encode as Encode
import JsonApi
import JsonApi.Decode
import JsonApi.Resources
import JsonApi.Documents
import Types exposing (..)


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


{-| Infix notation for Result.andThen. Makes andThen-chains look nicer.
-}
infixl 0 :>
(:>) : Result x a -> (a -> Result x b) -> Result x b
(:>) =
    Result.andThen


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
    getMe accessToken
        |> Task.perform AuthFailed GotMe
        |> Cmd.map wrapMsg


createProposalCmd : ProposalInput -> String -> (Msg -> a) -> Cmd a
createProposalCmd proposalInput accessToken wrapMsg =
    postProposal proposalInput accessToken
        |> Task.perform ProposalCreationFailed ProposalCreated
        |> Cmd.map wrapMsg


getProposalCmd : String -> String -> (Msg -> a) -> Cmd a
getProposalCmd id accessToken wrapMsg =
    getProposal id accessToken
        |> Task.perform GettingProposalFailed GotProposal
        |> Cmd.map wrapMsg



-- HTTP requests


httpWithHeader : String -> String -> Http.Request -> Http.Request
httpWithHeader field value request =
    { request | headers = ( field, value ) :: request.headers }


withAccessToken : String -> Http.Request -> Http.Request
withAccessToken accessToken =
    httpWithHeader "Authorization" ("Bearer " ++ accessToken)


requestGet : String -> String -> Http.Request
requestGet accessToken url =
    withAccessToken accessToken
        { verb = "GET"
        , headers = []
        , url = url
        , body = Http.empty
        }


requestPost : String -> String -> String -> Http.Request
requestPost accessToken url body =
    withAccessToken accessToken
        { verb = "POST"
        , headers = []
        , url = url
        , body = Http.string body
        }


sendJsonApi :
    (JsonApi.Document -> Result String a)
    -> Http.Request
    -> Task Http.Error a
sendJsonApi assembleResponse request =
    httpSendJsonApi assembleResponse Http.defaultSettings request


{-| Send a Http request and decode the response from a JSON API document
-}
httpSendJsonApi :
    (JsonApi.Document -> Result String a)
    -> Http.Settings
    -> Http.Request
    -> Task Http.Error a
httpSendJsonApi assembleResponse settings request =
    Http.send settings
        (request
            |> httpWithHeader "Content-Type" "application/vnd.api+json"
            |> httpWithHeader "Accept" "application/vnd.api+json"
        )
        |> Http.fromJson
            (Decode.customDecoder JsonApi.Decode.document assembleResponse)


exchangeAuthCodeForToken : String -> Task Http.Error String
exchangeAuthCodeForToken body =
    { verb = "POST"
    , headers = [ ( "Content-Type", "application/json" ) ]
    , url = tokenEndpoint
    , body = Http.string body
    }
        |> Http.send Http.defaultSettings
        |> Http.fromJson decodeToken


postProposal : ProposalInput -> String -> Task Http.Error Proposal
postProposal proposalInput accessToken =
    encodeProposalInput proposalInput
        |> requestPost accessToken newProposalEndpoint
        |> sendJsonApi assembleProposal


getProposal : String -> String -> Task Http.Error {- Maybe -} Proposal
getProposal id accessToken =
    getProposalEndpoint id
        |> requestGet accessToken
        |> sendJsonApi assembleProposal


getMe : String -> Task Http.Error Me
getMe accessToken =
    meEndpoint
        |> requestGet accessToken
        |> sendJsonApi assembleMe
