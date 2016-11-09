module Api.Util exposing (..)

import Task exposing (Task)
import Http
import JsonApi
import JsonApi.Extra


{-| Infix notation for Result.andThen. Makes andThen-chains look nicer.
-}
infixl 0 :>
(:>) : Result x a -> (a -> Result x b) -> Result x b
(:>) =
    Result.andThen


{-| Insert accessToken into Http header
-}
withAccessToken : String -> Http.Request -> Http.Request
withAccessToken accessToken =
    JsonApi.Extra.httpWithHeader "Authorization" ("Bearer " ++ accessToken)


{-| Build a GET request with an accessToken
-}
requestGet : String -> String -> Http.Request
requestGet accessToken url =
    withAccessToken accessToken
        { verb = "GET"
        , headers = []
        , url = url
        , body = Http.empty
        }


{-| Build a POST request with an accessToken
-}
requestPost : String -> String -> String -> Http.Request
requestPost accessToken url body =
    withAccessToken accessToken
        { verb = "POST"
        , headers = []
        , url = url
        , body = Http.string body
        }


{-| Send a Http request with default settings
and decode the response from a JSON API document
-}
sendJsonApi :
    (JsonApi.Document -> Result String a)
    -> Http.Request
    -> Task Http.Error a
sendJsonApi assembleResponse request =
    JsonApi.Extra.httpSendJsonApi assembleResponse Http.defaultSettings request
