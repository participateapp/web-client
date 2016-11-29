module JsonApi.Extra
    exposing
        ( withHeader
        , sendJsonApi
        , ResourceLinkage
        , resourceLinkage
        , resourceLinkageCollection
        , encodeDocument
        )

import Json.Decode as Decode
import Json.Encode as Encode
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


{-| Represents a resource linkage
-}
type ResourceLinkage
    = ResourceLinkageOne (Maybe ( String, String ))
    | ResourceLinkageMany (List ( String, String ))


resourceLinkage : Maybe ( String, String ) -> ResourceLinkage
resourceLinkage =
    ResourceLinkageOne


resourceLinkageCollection : List ( String, String ) -> ResourceLinkage
resourceLinkageCollection =
    ResourceLinkageMany


encodeDocument :
    String
    -> List ( String, Encode.Value )
    -> List ( String, ResourceLinkage )
    -> String
encodeDocument resourceType attributes relationships =
    let
        encodedType : ( String, Encode.Value )
        encodedType =
            ( "type", Encode.string resourceType )

        encodedAttributes : Maybe ( String, Encode.Value )
        encodedAttributes =
            if List.isEmpty attributes then
                Nothing
            else
                Just ( "attributes", Encode.object attributes )

        encodedRelationships : Maybe ( String, Encode.Value )
        encodedRelationships =
            if List.isEmpty relationships then
                Nothing
            else
                Just ( "relationships", Encode.object <| List.map encodeRelationship relationships )

        encodeRelationship : ( String, ResourceLinkage ) -> ( String, Encode.Value )
        encodeRelationship ( name, linkage ) =
            ( name, Encode.object [ ( "data", encodeLinkage linkage ) ] )

        encodeLinkage : ResourceLinkage -> Encode.Value
        encodeLinkage linkage =
            case linkage of
                ResourceLinkageOne Nothing ->
                    Encode.null

                ResourceLinkageOne (Just typeAndId) ->
                    encodeIdentifier typeAndId

                ResourceLinkageMany list ->
                    Encode.list <| List.map encodeIdentifier list

        encodeIdentifier : ( String, String ) -> Encode.Value
        encodeIdentifier ( linkageType, linkageId ) =
            Encode.object [ ( "type", Encode.string linkageType ), ( "id", Encode.string linkageId ) ]
    in
        Encode.object
            [ ( "data"
              , Encode.object <|
                    List.filterMap identity <|
                        [ Just encodedType
                        , encodedAttributes
                        , encodedRelationships
                        ]
              )
            ]
            |> Encode.encode 0
