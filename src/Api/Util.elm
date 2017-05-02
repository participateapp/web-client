module Api.Util exposing (..)

import Result.Extra
import Task exposing (Task)
import Json.Encode as Encode
import Json.Decode as Decode
import Http
import HttpBuilder
import JsonApi
import JsonApi.Decode


{-| Infix notation for Result.andThen. Makes andThen-chains look nicer.
-}
infixl 0 :>
(:>) : Result x a -> (a -> Result x b) -> Result x b
(:>) =
    flip Result.andThen


{-| Convenience version of `Task.attempt` that performs Tasks that may fail.
That's the same as in `elm-lang/core 4.x` (Elm 0.17)
-}
attempt : (e -> msg) -> (a -> msg) -> Task e a -> Cmd msg
attempt errorTagger successTagger task =
    Task.attempt (Result.Extra.unpack errorTagger successTagger) task


{-| Insert accessToken into Http header
-}
withAccessToken : String -> HttpBuilder.RequestBuilder a -> HttpBuilder.RequestBuilder a
withAccessToken accessToken =
    HttpBuilder.withHeader "Authorization" ("Bearer " ++ accessToken)


{-| Insert a JSON value as the body of a RequestBuilder.
This will set the `Content-Type: application/vnd.api+json` header,
as required by JSON API standard.
-}
withJsonApiBody : Encode.Value -> HttpBuilder.RequestBuilder a -> HttpBuilder.RequestBuilder a
withJsonApiBody jsonValue =
    HttpBuilder.withBody <|
        Http.stringBody "application/vnd.api+json" (Encode.encode 0 jsonValue)


{-| Expect the response body to be a JsonApi Document.
-}
withExpectJsonApi :
    (JsonApi.Document -> Result String a)
    -> HttpBuilder.RequestBuilder ()
    -> HttpBuilder.RequestBuilder a
withExpectJsonApi assembleResponse requestBuilder =
    requestBuilder
        |> HttpBuilder.withHeader "Accept" "application/vnd.api+json"
        |> HttpBuilder.withExpect
            (Http.expectJson
                (JsonApi.Decode.document
                    |> Decode.andThen
                        (\document ->
                            case assembleResponse document of
                                Ok successValue ->
                                    Decode.succeed successValue

                                Err errorMessage ->
                                    Decode.fail errorMessage
                        )
                )
            )
