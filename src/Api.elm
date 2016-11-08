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


decoderAssertType : List String -> String -> Decoder a -> Decoder a
decoderAssertType path expectedType decoder =
    Decode.at path Decode.string
        `Decode.andThen`
            \actualType ->
                if actualType == expectedType then
                    decoder
                else
                    Decode.fail <|
                        "Expected type \""
                            ++ expectedType
                            ++ "\", got \""
                            ++ actualType
                            ++ "\""


decodeToken : Decoder String
decodeToken =
    Decode.at [ "access_token" ] Decode.string


decodeMe : Decoder Me
decodeMe =
    Decode.at [ "data" ] <|
        decoderAssertType [ "type" ] "participant" <|
            Decode.object1 Me
                (Decode.at [ "attributes", "name" ] Decode.string)


decodeProposal : Decoder Proposal
decodeProposal =
    decoderAssertType [ "data", "type" ] "proposal" <|
        Decode.object4
            Proposal
            (Decode.at [ "data", "id" ] Decode.string)
            (Decode.at [ "data", "attributes", "title" ] Decode.string)
            (Decode.at [ "data", "attributes", "body" ] Decode.string)
            (Decode.at [ "data", "relationships", "author", "data" ] <|
                decoderAssertType [ "type" ] "participant" <|
                    Decode.object2
                        Participant
                        (Decode.at [ "id" ] Decode.string)
                        (Decode.succeed "author-lookup-not-yet-implemented")
             -- will be done by elm-jsonapi
            )


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
        |> Http.fromJson decodeProposal


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
        |> Http.fromJson decodeProposal


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
