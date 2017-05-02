module JsonApi.Extra
    exposing
        ( ResourceLinkage
        , resourceLinkage
        , resourceLinkageCollection
        , encodeDocument
        )

import Json.Decode as Decode
import Json.Encode as Encode
import Task exposing (Task)
import Http
import JsonApi


{-| Represents a resource linkage
-}
type ResourceLinkage
    = ResourceLinkageOne (Maybe ( String, String ))
    | ResourceLinkageMany (List ( String, String ))


{-| Build a to-one resource linkage by type and id
of the related resource
-}
resourceLinkage : Maybe ( String, String ) -> ResourceLinkage
resourceLinkage =
    ResourceLinkageOne


{-| Build a to-many resource linkage from a list of type-id-pairs
-}
resourceLinkageCollection : List ( String, String ) -> ResourceLinkage
resourceLinkageCollection =
    ResourceLinkageMany


{-| Encode a resource object for request to create or update a resource.

The resource is specified by:
- a resource type
- an optional client-generated id
- a list of attributes
- a list of relationships

Returns a JSON value suitable for a POST or PATCH request.
-}
encodeDocument :
    String
    -> Maybe String
    -> List ( String, Encode.Value )
    -> List ( String, ResourceLinkage )
    -> Encode.Value
encodeDocument resourceType optionalId attributes relationships =
    let
        encodedType : ( String, Encode.Value )
        encodedType =
            ( "type", Encode.string resourceType )

        encodedId : Maybe ( String, Encode.Value )
        encodedId =
            Maybe.map
                (\id -> ( "id", Encode.string id ))
                optionalId

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
                        -- skips unused elements
                        [ Just encodedType
                        , encodedId
                        , encodedAttributes
                        , encodedRelationships
                        ]
              )
            ]
