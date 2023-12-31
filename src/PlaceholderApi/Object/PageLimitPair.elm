-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module PlaceholderApi.Object.PageLimitPair exposing (..)

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


page : SelectionSet (Maybe Int) PlaceholderApi.Object.PageLimitPair
page =
    Object.selectionForField "(Maybe Int)" "page" [] (Decode.int |> Decode.nullable)


limit : SelectionSet (Maybe Int) PlaceholderApi.Object.PageLimitPair
limit =
    Object.selectionForField "(Maybe Int)" "limit" [] (Decode.int |> Decode.nullable)
