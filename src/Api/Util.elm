module Api.Util exposing (..)

import Result.Extra
import Task exposing (Task)
import Json.Decode as Decode
import Http
import JsonApi
import JsonApi.Extra


{-| Infix notation for Result.andThen. Makes andThen-chains look nicer.
-}



-- ToDo: Elm 0.18 encourages using
--            |> Result.andThen
--       Compare this style (as soon the ongoing Elm 0.18 port is type-checking)
--       to the infix version here.


infixl 0 :>
(:>) : Result x a -> (a -> Result x b) -> Result x b
(:>) =
    flip Result.andThen


{-| Convenience version of `Task.attempt` that performs Tasks that may fail.
That's the same as in `elm-lang/core 4.x` (Elm 0.17)
-}
attempt : (e -> msg) -> (a -> msg) -> Task e a -> Cmd msg
attempt errorTagger successTagger task =
    Task.attempt (Result.extra.unpack errorTagger successTagger) task


{-| Insert accessToken into Http header
-}
withAccessToken : String -> Http.Request -> Http.Request
withAccessToken accessToken =
    JsonApi.Extra.withHeader "Authorization" ("Bearer " ++ accessToken)


{-| Build a GET request
-}
requestGet : String -> Http.Request
requestGet url =
    { verb = "GET"
    , headers = []
    , url = url
    , body = Http.empty
    }


{-| Build a POST request
-}
requestPost : String -> String -> Http.Request
requestPost url body =
    { verb = "POST"
    , headers = []
    , url = url
    , body = Http.string body
    }


{-| Send a Http request with default settings
and decode the response from a JSON API document
-}
sendDefJsonApi :
    (JsonApi.Document -> Result String a)
    -> Http.Request
    -> Task Http.Error a
sendDefJsonApi assembleResponse request =
    JsonApi.Extra.sendJsonApi assembleResponse Http.defaultSettings request


{-| Send a Http request with default settings
and decode the response from JSON
-}
sendDefJson :
    Decode.Decoder a
    -> Http.Request
    -> Task Http.Error a
sendDefJson decodeResponse request =
    request
        |> Http.send Http.defaultSettings
        |> Http.fromJson decodeResponse
