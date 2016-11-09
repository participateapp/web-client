module JsonApi.Extra exposing (..)

import Json.Decode as Decode
import Task exposing (Task)
import Http
import JsonApi
import JsonApi.Decode


httpWithHeader : String -> String -> Http.Request -> Http.Request
httpWithHeader field value request =
    { request | headers = ( field, value ) :: request.headers }


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
