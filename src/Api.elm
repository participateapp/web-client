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


decodeMe : Decoder Me
decodeMe =
    Decode.at [ "data" ] <|
        Decode.object1 Me
            (Decode.at [ "attributes", "name" ] Decode.string)


type alias ProposalAttributes =
    { title : String
    , body : String
    }


decodeProposalAttributes : Decoder ProposalAttributes
decodeProposalAttributes =
    Decode.object2 ProposalAttributes
        (Decode.at [ "title" ] Decode.string)
        (Decode.at [ "body" ] Decode.string)


type alias ParticipantAttributes =
    { name : String
    }


decodeParticipantAttributes : Decoder ParticipantAttributes
decodeParticipantAttributes =
    Decode.object1 ParticipantAttributes
        (Decode.at [ "name" ] Decode.string)


httpFromJsonApi :
    (JsonApi.Document -> Result String a)
    -> Task Http.RawError Http.Response
    -> Task Http.Error a
httpFromJsonApi assembler response =
    Http.fromJson
        (Decode.customDecoder JsonApi.Decode.document assembler)
        response


infixl 0 |:


(:>) =
    Result.andThen


assembleProposal : JsonApi.Document -> Result String Proposal
assembleProposal document =
    JsonApi.Documents.primaryResource document
        :> \proposalResource ->
            JsonApi.Resources.attributes decodeProposalAttributes proposalResource
                :> \{ title, body } ->
                    JsonApi.Resources.relatedResource "author" proposalResource
                        :> \participantResource ->
                            JsonApi.Resources.attributes decodeParticipantAttributes participantResource
                                :> \{ name } ->
                                    Ok
                                        { id = JsonApi.Resources.id proposalResource
                                        , title = title
                                        , body = body
                                        , author =
                                            { id = JsonApi.Resources.id participantResource
                                            , name = name
                                            }
                                        }


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
    getWithToken meEndpoint accessToken decodeMe
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


exchangeAuthCodeForToken : String -> Task Http.Error String
exchangeAuthCodeForToken body =
    { verb = "POST", headers = [ ( "Content-Type", "application/json" ) ], url = tokenEndpoint, body = Http.string body }
        |> Http.send Http.defaultSettings
        |> Http.fromJson decodeToken


postProposal : ProposalInput -> String -> Task Http.Error Proposal
postProposal proposalInput accessToken =
    { verb = "POST"
    , headers =
        [ ( "Authorization", "Bearer " ++ accessToken )
        , ( "Content-Type", "application/vnd.api+json" )
        ]
    , url = newProposalEndpoint
    , body = Http.string (encodeProposalInput proposalInput)
    }
        |> Http.send Http.defaultSettings
        |> httpFromJsonApi assembleProposal


getProposal : String -> String -> Task Http.Error {- Maybe -} Proposal
getProposal id accessToken =
    { verb = "GET"
    , headers =
        [ ( "Authorization", "Bearer " ++ accessToken )
        , ( "Content-Type", "application/vnd.api+json" )
        ]
    , url = getProposalEndpoint id
    , body = Http.empty
    }
        |> Http.send Http.defaultSettings
        |> httpFromJsonApi assembleProposal



-- |> Http.fromJson JsonApi.Decode.document
-- |>


getWithToken : String -> String -> Decoder a -> Task Http.Error a
getWithToken url accessToken responseDecoder =
    { verb = "GET"
    , headers =
        [ ( "Authorization", "Bearer " ++ accessToken )
        , ( "Content-Type", "application/vnd.api+json" )
        ]
    , url = url
    , body = Http.empty
    }
        |> Http.send Http.defaultSettings
        |> Http.fromJson responseDecoder
