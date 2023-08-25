-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module PlaceholderApi.Object.Comment exposing (..)

import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode as Encode exposing (Value)
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Decode as Decode
import PlaceholderApi.InputObject
import PlaceholderApi.Interface
import PlaceholderApi.Object
import PlaceholderApi.Scalar
import PlaceholderApi.ScalarCodecs
import PlaceholderApi.Union


id : SelectionSet (Maybe PlaceholderApi.ScalarCodecs.Id) PlaceholderApi.Object.Comment
id =
    Object.selectionForField "(Maybe ScalarCodecs.Id)" "id" [] (PlaceholderApi.ScalarCodecs.codecs |> PlaceholderApi.Scalar.unwrapCodecs |> .codecId |> .decoder |> Decode.nullable)


name : SelectionSet (Maybe String) PlaceholderApi.Object.Comment
name =
    Object.selectionForField "(Maybe String)" "name" [] (Decode.string |> Decode.nullable)


email : SelectionSet (Maybe String) PlaceholderApi.Object.Comment
email =
    Object.selectionForField "(Maybe String)" "email" [] (Decode.string |> Decode.nullable)


body : SelectionSet (Maybe String) PlaceholderApi.Object.Comment
body =
    Object.selectionForField "(Maybe String)" "body" [] (Decode.string |> Decode.nullable)


post :
    SelectionSet decodesTo PlaceholderApi.Object.Post
    -> SelectionSet (Maybe decodesTo) PlaceholderApi.Object.Comment
post object____ =
    Object.selectionForCompositeField "post" [] object____ (Basics.identity >> Decode.nullable)