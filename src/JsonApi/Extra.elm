module JsonApi.Extra exposing (..)

import Json.Decode as Decode
import Task exposing (Task)
import Http
import JsonApi
import JsonApi.Decode


{-| Insert a header field into a Http request
-}
withHeader : String -> String -> Http.Request -> Http.Request
withHeader field value request =
    { request | headers = ( field, value ) :: request.headers }


{-| Send a Http request and decode the response from a JSON API document
-}
sendJsonApi :
    (JsonApi.Document -> Result String a)
    -> Http.Settings
    -> Http.Request
    -> Task Http.Error a
sendJsonApi assembleResponse settings request =
    Http.send settings
        (request
            |> withHeader "Content-Type" "application/vnd.api+json"
            |> withHeader "Accept" "application/vnd.api+json"
        )
        |> Http.fromJson
            (Decode.customDecoder JsonApi.Decode.document assembleResponse)
