-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module PlaceholderApi.InputObject exposing (..)

import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode as Encode exposing (Value)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Decode as Decode
import PlaceholderApi.Enum.OperatorKindEnum
import PlaceholderApi.Enum.SortOrderEnum
import PlaceholderApi.Interface
import PlaceholderApi.Object
import PlaceholderApi.Scalar
import PlaceholderApi.ScalarCodecs
import PlaceholderApi.Union


buildAddressInput :
    (AddressInputOptionalFields -> AddressInputOptionalFields)
    -> AddressInput
buildAddressInput fillOptionals____ =
    let
        optionals____ =
            fillOptionals____
                { street = Absent, suite = Absent, city = Absent, zipcode = Absent, geo = Absent }
    in
    { street = optionals____.street, suite = optionals____.suite, city = optionals____.city, zipcode = optionals____.zipcode, geo = optionals____.geo }


type alias AddressInputOptionalFields =
    { street : OptionalArgument String
    , suite : OptionalArgument String
    , city : OptionalArgument String
    , zipcode : OptionalArgument String
    , geo : OptionalArgument GeoInput
    }


{-| Type for the AddressInput input object.
-}
type alias AddressInput =
    { street : OptionalArgument String
    , suite : OptionalArgument String
    , city : OptionalArgument String
    , zipcode : OptionalArgument String
    , geo : OptionalArgument GeoInput
    }


{-| Encode a AddressInput into a value that can be used as an argument.
-}
encodeAddressInput : AddressInput -> Value
encodeAddressInput input____ =
    Encode.maybeObject
        [ ( "street", Encode.string |> Encode.optional input____.street ), ( "suite", Encode.string |> Encode.optional input____.suite ), ( "city", Encode.string |> Encode.optional input____.city ), ( "zipcode", Encode.string |> Encode.optional input____.zipcode ), ( "geo", encodeGeoInput |> Encode.optional input____.geo ) ]


buildCompanyInput :
    (CompanyInputOptionalFields -> CompanyInputOptionalFields)
    -> CompanyInput
buildCompanyInput fillOptionals____ =
    let
        optionals____ =
            fillOptionals____
                { name = Absent, catchPhrase = Absent, bs = Absent }
    in
    { name = optionals____.name, catchPhrase = optionals____.catchPhrase, bs = optionals____.bs }


type alias CompanyInputOptionalFields =
    { name : OptionalArgument String
    , catchPhrase : OptionalArgument String
    , bs : OptionalArgument String
    }


{-| Type for the CompanyInput input object.
-}
type alias CompanyInput =
    { name : OptionalArgument String
    , catchPhrase : OptionalArgument String
    , bs : OptionalArgument String
    }


{-| Encode a CompanyInput into a value that can be used as an argument.
-}
encodeCompanyInput : CompanyInput -> Value
encodeCompanyInput input____ =
    Encode.maybeObject
        [ ( "name", Encode.string |> Encode.optional input____.name ), ( "catchPhrase", Encode.string |> Encode.optional input____.catchPhrase ), ( "bs", Encode.string |> Encode.optional input____.bs ) ]


buildCreateAlbumInput :
    CreateAlbumInputRequiredFields
    -> CreateAlbumInput
buildCreateAlbumInput required____ =
    { title = required____.title, userId = required____.userId }


type alias CreateAlbumInputRequiredFields =
    { title : String
    , userId : PlaceholderApi.ScalarCodecs.Id
    }


{-| Type for the CreateAlbumInput input object.
-}
type alias CreateAlbumInput =
    { title : String
    , userId : PlaceholderApi.ScalarCodecs.Id
    }


{-| Encode a CreateAlbumInput into a value that can be used as an argument.
-}
encodeCreateAlbumInput : CreateAlbumInput -> Value
encodeCreateAlbumInput input____ =
    Encode.maybeObject
        [ ( "title", Encode.string input____.title |> Just ), ( "userId", (PlaceholderApi.ScalarCodecs.codecs |> PlaceholderApi.Scalar.unwrapEncoder .codecId) input____.userId |> Just ) ]


buildCreateCommentInput :
    CreateCommentInputRequiredFields
    -> CreateCommentInput
buildCreateCommentInput required____ =
    { name = required____.name, email = required____.email, body = required____.body }


type alias CreateCommentInputRequiredFields =
    { name : String
    , email : String
    , body : String
    }


{-| Type for the CreateCommentInput input object.
-}
type alias CreateCommentInput =
    { name : String
    , email : String
    , body : String
    }


{-| Encode a CreateCommentInput into a value that can be used as an argument.
-}
encodeCreateCommentInput : CreateCommentInput -> Value
encodeCreateCommentInput input____ =
    Encode.maybeObject
        [ ( "name", Encode.string input____.name |> Just ), ( "email", Encode.string input____.email |> Just ), ( "body", Encode.string input____.body |> Just ) ]


buildCreatePhotoInput :
    CreatePhotoInputRequiredFields
    -> CreatePhotoInput
buildCreatePhotoInput required____ =
    { title = required____.title, url = required____.url, thumbnailUrl = required____.thumbnailUrl }


type alias CreatePhotoInputRequiredFields =
    { title : String
    , url : String
    , thumbnailUrl : String
    }


{-| Type for the CreatePhotoInput input object.
-}
type alias CreatePhotoInput =
    { title : String
    , url : String
    , thumbnailUrl : String
    }


{-| Encode a CreatePhotoInput into a value that can be used as an argument.
-}
encodeCreatePhotoInput : CreatePhotoInput -> Value
encodeCreatePhotoInput input____ =
    Encode.maybeObject
        [ ( "title", Encode.string input____.title |> Just ), ( "url", Encode.string input____.url |> Just ), ( "thumbnailUrl", Encode.string input____.thumbnailUrl |> Just ) ]


buildCreatePostInput :
    CreatePostInputRequiredFields
    -> CreatePostInput
buildCreatePostInput required____ =
    { title = required____.title, body = required____.body }


type alias CreatePostInputRequiredFields =
    { title : String
    , body : String
    }


{-| Type for the CreatePostInput input object.
-}
type alias CreatePostInput =
    { title : String
    , body : String
    }


{-| Encode a CreatePostInput into a value that can be used as an argument.
-}
encodeCreatePostInput : CreatePostInput -> Value
encodeCreatePostInput input____ =
    Encode.maybeObject
        [ ( "title", Encode.string input____.title |> Just ), ( "body", Encode.string input____.body |> Just ) ]


buildCreateTodoInput :
    CreateTodoInputRequiredFields
    -> CreateTodoInput
buildCreateTodoInput required____ =
    { title = required____.title, completed = required____.completed }


type alias CreateTodoInputRequiredFields =
    { title : String
    , completed : Bool
    }


{-| Type for the CreateTodoInput input object.
-}
type alias CreateTodoInput =
    { title : String
    , completed : Bool
    }


{-| Encode a CreateTodoInput into a value that can be used as an argument.
-}
encodeCreateTodoInput : CreateTodoInput -> Value
encodeCreateTodoInput input____ =
    Encode.maybeObject
        [ ( "title", Encode.string input____.title |> Just ), ( "completed", Encode.bool input____.completed |> Just ) ]


buildCreateUserInput :
    CreateUserInputRequiredFields
    -> (CreateUserInputOptionalFields -> CreateUserInputOptionalFields)
    -> CreateUserInput
buildCreateUserInput required____ fillOptionals____ =
    let
        optionals____ =
            fillOptionals____
                { address = Absent, phone = Absent, website = Absent, company = Absent }
    in
    { name = required____.name, username = required____.username, email = required____.email, address = optionals____.address, phone = optionals____.phone, website = optionals____.website, company = optionals____.company }


type alias CreateUserInputRequiredFields =
    { name : String
    , username : String
    , email : String
    }


type alias CreateUserInputOptionalFields =
    { address : OptionalArgument AddressInput
    , phone : OptionalArgument String
    , website : OptionalArgument String
    , company : OptionalArgument CompanyInput
    }


{-| Type for the CreateUserInput input object.
-}
type alias CreateUserInput =
    { name : String
    , username : String
    , email : String
    , address : OptionalArgument AddressInput
    , phone : OptionalArgument String
    , website : OptionalArgument String
    , company : OptionalArgument CompanyInput
    }


{-| Encode a CreateUserInput into a value that can be used as an argument.
-}
encodeCreateUserInput : CreateUserInput -> Value
encodeCreateUserInput input____ =
    Encode.maybeObject
        [ ( "name", Encode.string input____.name |> Just ), ( "username", Encode.string input____.username |> Just ), ( "email", Encode.string input____.email |> Just ), ( "address", encodeAddressInput |> Encode.optional input____.address ), ( "phone", Encode.string |> Encode.optional input____.phone ), ( "website", Encode.string |> Encode.optional input____.website ), ( "company", encodeCompanyInput |> Encode.optional input____.company ) ]


buildGeoInput :
    (GeoInputOptionalFields -> GeoInputOptionalFields)
    -> GeoInput
buildGeoInput fillOptionals____ =
    let
        optionals____ =
            fillOptionals____
                { lat = Absent, lng = Absent }
    in
    { lat = optionals____.lat, lng = optionals____.lng }


type alias GeoInputOptionalFields =
    { lat : OptionalArgument Float
    , lng : OptionalArgument Float
    }


{-| Type for the GeoInput input object.
-}
type alias GeoInput =
    { lat : OptionalArgument Float
    , lng : OptionalArgument Float
    }


{-| Encode a GeoInput into a value that can be used as an argument.
-}
encodeGeoInput : GeoInput -> Value
encodeGeoInput input____ =
    Encode.maybeObject
        [ ( "lat", Encode.float |> Encode.optional input____.lat ), ( "lng", Encode.float |> Encode.optional input____.lng ) ]


buildOperatorOptions :
    (OperatorOptionsOptionalFields -> OperatorOptionsOptionalFields)
    -> OperatorOptions
buildOperatorOptions fillOptionals____ =
    let
        optionals____ =
            fillOptionals____
                { kind = Absent, field = Absent, value = Absent }
    in
    { kind = optionals____.kind, field = optionals____.field, value = optionals____.value }


type alias OperatorOptionsOptionalFields =
    { kind : OptionalArgument PlaceholderApi.Enum.OperatorKindEnum.OperatorKindEnum
    , field : OptionalArgument String
    , value : OptionalArgument String
    }


{-| Type for the OperatorOptions input object.
-}
type alias OperatorOptions =
    { kind : OptionalArgument PlaceholderApi.Enum.OperatorKindEnum.OperatorKindEnum
    , field : OptionalArgument String
    , value : OptionalArgument String
    }


{-| Encode a OperatorOptions into a value that can be used as an argument.
-}
encodeOperatorOptions : OperatorOptions -> Value
encodeOperatorOptions input____ =
    Encode.maybeObject
        [ ( "kind", Encode.enum PlaceholderApi.Enum.OperatorKindEnum.toString |> Encode.optional input____.kind ), ( "field", Encode.string |> Encode.optional input____.field ), ( "value", Encode.string |> Encode.optional input____.value ) ]


buildPageQueryOptions :
    (PageQueryOptionsOptionalFields -> PageQueryOptionsOptionalFields)
    -> PageQueryOptions
buildPageQueryOptions fillOptionals____ =
    let
        optionals____ =
            fillOptionals____
                { paginate = Absent, slice = Absent, sort = Absent, operators = Absent, search = Absent }
    in
    { paginate = optionals____.paginate, slice = optionals____.slice, sort = optionals____.sort, operators = optionals____.operators, search = optionals____.search }


type alias PageQueryOptionsOptionalFields =
    { paginate : OptionalArgument PaginateOptions
    , slice : OptionalArgument SliceOptions
    , sort : OptionalArgument (List (Maybe SortOptions))
    , operators : OptionalArgument (List (Maybe OperatorOptions))
    , search : OptionalArgument SearchOptions
    }


{-| Type for the PageQueryOptions input object.
-}
type alias PageQueryOptions =
    { paginate : OptionalArgument PaginateOptions
    , slice : OptionalArgument SliceOptions
    , sort : OptionalArgument (List (Maybe SortOptions))
    , operators : OptionalArgument (List (Maybe OperatorOptions))
    , search : OptionalArgument SearchOptions
    }


{-| Encode a PageQueryOptions into a value that can be used as an argument.
-}
encodePageQueryOptions : PageQueryOptions -> Value
encodePageQueryOptions input____ =
    Encode.maybeObject
        [ ( "paginate", encodePaginateOptions |> Encode.optional input____.paginate ), ( "slice", encodeSliceOptions |> Encode.optional input____.slice ), ( "sort", (encodeSortOptions |> Encode.maybe |> Encode.list) |> Encode.optional input____.sort ), ( "operators", (encodeOperatorOptions |> Encode.maybe |> Encode.list) |> Encode.optional input____.operators ), ( "search", encodeSearchOptions |> Encode.optional input____.search ) ]


buildPaginateOptions :
    (PaginateOptionsOptionalFields -> PaginateOptionsOptionalFields)
    -> PaginateOptions
buildPaginateOptions fillOptionals____ =
    let
        optionals____ =
            fillOptionals____
                { page = Absent, limit = Absent }
    in
    { page = optionals____.page, limit = optionals____.limit }


type alias PaginateOptionsOptionalFields =
    { page : OptionalArgument Int
    , limit : OptionalArgument Int
    }


{-| Type for the PaginateOptions input object.
-}
type alias PaginateOptions =
    { page : OptionalArgument Int
    , limit : OptionalArgument Int
    }


{-| Encode a PaginateOptions into a value that can be used as an argument.
-}
encodePaginateOptions : PaginateOptions -> Value
encodePaginateOptions input____ =
    Encode.maybeObject
        [ ( "page", Encode.int |> Encode.optional input____.page ), ( "limit", Encode.int |> Encode.optional input____.limit ) ]


buildSearchOptions :
    (SearchOptionsOptionalFields -> SearchOptionsOptionalFields)
    -> SearchOptions
buildSearchOptions fillOptionals____ =
    let
        optionals____ =
            fillOptionals____
                { q = Absent }
    in
    { q = optionals____.q }


type alias SearchOptionsOptionalFields =
    { q : OptionalArgument String }


{-| Type for the SearchOptions input object.
-}
type alias SearchOptions =
    { q : OptionalArgument String }


{-| Encode a SearchOptions into a value that can be used as an argument.
-}
encodeSearchOptions : SearchOptions -> Value
encodeSearchOptions input____ =
    Encode.maybeObject
        [ ( "q", Encode.string |> Encode.optional input____.q ) ]


buildSliceOptions :
    (SliceOptionsOptionalFields -> SliceOptionsOptionalFields)
    -> SliceOptions
buildSliceOptions fillOptionals____ =
    let
        optionals____ =
            fillOptionals____
                { start = Absent, end = Absent, limit = Absent }
    in
    { start = optionals____.start, end = optionals____.end, limit = optionals____.limit }


type alias SliceOptionsOptionalFields =
    { start : OptionalArgument Int
    , end : OptionalArgument Int
    , limit : OptionalArgument Int
    }


{-| Type for the SliceOptions input object.
-}
type alias SliceOptions =
    { start : OptionalArgument Int
    , end : OptionalArgument Int
    , limit : OptionalArgument Int
    }


{-| Encode a SliceOptions into a value that can be used as an argument.
-}
encodeSliceOptions : SliceOptions -> Value
encodeSliceOptions input____ =
    Encode.maybeObject
        [ ( "start", Encode.int |> Encode.optional input____.start ), ( "end", Encode.int |> Encode.optional input____.end ), ( "limit", Encode.int |> Encode.optional input____.limit ) ]


buildSortOptions :
    (SortOptionsOptionalFields -> SortOptionsOptionalFields)
    -> SortOptions
buildSortOptions fillOptionals____ =
    let
        optionals____ =
            fillOptionals____
                { field = Absent, order = Absent }
    in
    { field = optionals____.field, order = optionals____.order }


type alias SortOptionsOptionalFields =
    { field : OptionalArgument String
    , order : OptionalArgument PlaceholderApi.Enum.SortOrderEnum.SortOrderEnum
    }


{-| Type for the SortOptions input object.
-}
type alias SortOptions =
    { field : OptionalArgument String
    , order : OptionalArgument PlaceholderApi.Enum.SortOrderEnum.SortOrderEnum
    }


{-| Encode a SortOptions into a value that can be used as an argument.
-}
encodeSortOptions : SortOptions -> Value
encodeSortOptions input____ =
    Encode.maybeObject
        [ ( "field", Encode.string |> Encode.optional input____.field ), ( "order", Encode.enum PlaceholderApi.Enum.SortOrderEnum.toString |> Encode.optional input____.order ) ]


buildUpdateAlbumInput :
    (UpdateAlbumInputOptionalFields -> UpdateAlbumInputOptionalFields)
    -> UpdateAlbumInput
buildUpdateAlbumInput fillOptionals____ =
    let
        optionals____ =
            fillOptionals____
                { title = Absent, userId = Absent }
    in
    { title = optionals____.title, userId = optionals____.userId }


type alias UpdateAlbumInputOptionalFields =
    { title : OptionalArgument String
    , userId : OptionalArgument PlaceholderApi.ScalarCodecs.Id
    }


{-| Type for the UpdateAlbumInput input object.
-}
type alias UpdateAlbumInput =
    { title : OptionalArgument String
    , userId : OptionalArgument PlaceholderApi.ScalarCodecs.Id
    }


{-| Encode a UpdateAlbumInput into a value that can be used as an argument.
-}
encodeUpdateAlbumInput : UpdateAlbumInput -> Value
encodeUpdateAlbumInput input____ =
    Encode.maybeObject
        [ ( "title", Encode.string |> Encode.optional input____.title ), ( "userId", (PlaceholderApi.ScalarCodecs.codecs |> PlaceholderApi.Scalar.unwrapEncoder .codecId) |> Encode.optional input____.userId ) ]


buildUpdateCommentInput :
    (UpdateCommentInputOptionalFields -> UpdateCommentInputOptionalFields)
    -> UpdateCommentInput
buildUpdateCommentInput fillOptionals____ =
    let
        optionals____ =
            fillOptionals____
                { name = Absent, email = Absent, body = Absent }
    in
    { name = optionals____.name, email = optionals____.email, body = optionals____.body }


type alias UpdateCommentInputOptionalFields =
    { name : OptionalArgument String
    , email : OptionalArgument String
    , body : OptionalArgument String
    }


{-| Type for the UpdateCommentInput input object.
-}
type alias UpdateCommentInput =
    { name : OptionalArgument String
    , email : OptionalArgument String
    , body : OptionalArgument String
    }


{-| Encode a UpdateCommentInput into a value that can be used as an argument.
-}
encodeUpdateCommentInput : UpdateCommentInput -> Value
encodeUpdateCommentInput input____ =
    Encode.maybeObject
        [ ( "name", Encode.string |> Encode.optional input____.name ), ( "email", Encode.string |> Encode.optional input____.email ), ( "body", Encode.string |> Encode.optional input____.body ) ]


buildUpdatePhotoInput :
    (UpdatePhotoInputOptionalFields -> UpdatePhotoInputOptionalFields)
    -> UpdatePhotoInput
buildUpdatePhotoInput fillOptionals____ =
    let
        optionals____ =
            fillOptionals____
                { title = Absent, url = Absent, thumbnailUrl = Absent }
    in
    { title = optionals____.title, url = optionals____.url, thumbnailUrl = optionals____.thumbnailUrl }


type alias UpdatePhotoInputOptionalFields =
    { title : OptionalArgument String
    , url : OptionalArgument String
    , thumbnailUrl : OptionalArgument String
    }


{-| Type for the UpdatePhotoInput input object.
-}
type alias UpdatePhotoInput =
    { title : OptionalArgument String
    , url : OptionalArgument String
    , thumbnailUrl : OptionalArgument String
    }


{-| Encode a UpdatePhotoInput into a value that can be used as an argument.
-}
encodeUpdatePhotoInput : UpdatePhotoInput -> Value
encodeUpdatePhotoInput input____ =
    Encode.maybeObject
        [ ( "title", Encode.string |> Encode.optional input____.title ), ( "url", Encode.string |> Encode.optional input____.url ), ( "thumbnailUrl", Encode.string |> Encode.optional input____.thumbnailUrl ) ]


buildUpdatePostInput :
    (UpdatePostInputOptionalFields -> UpdatePostInputOptionalFields)
    -> UpdatePostInput
buildUpdatePostInput fillOptionals____ =
    let
        optionals____ =
            fillOptionals____
                { title = Absent, body = Absent }
    in
    { title = optionals____.title, body = optionals____.body }


type alias UpdatePostInputOptionalFields =
    { title : OptionalArgument String
    , body : OptionalArgument String
    }


{-| Type for the UpdatePostInput input object.
-}
type alias UpdatePostInput =
    { title : OptionalArgument String
    , body : OptionalArgument String
    }


{-| Encode a UpdatePostInput into a value that can be used as an argument.
-}
encodeUpdatePostInput : UpdatePostInput -> Value
encodeUpdatePostInput input____ =
    Encode.maybeObject
        [ ( "title", Encode.string |> Encode.optional input____.title ), ( "body", Encode.string |> Encode.optional input____.body ) ]


buildUpdateTodoInput :
    (UpdateTodoInputOptionalFields -> UpdateTodoInputOptionalFields)
    -> UpdateTodoInput
buildUpdateTodoInput fillOptionals____ =
    let
        optionals____ =
            fillOptionals____
                { title = Absent, completed = Absent }
    in
    { title = optionals____.title, completed = optionals____.completed }


type alias UpdateTodoInputOptionalFields =
    { title : OptionalArgument String
    , completed : OptionalArgument Bool
    }


{-| Type for the UpdateTodoInput input object.
-}
type alias UpdateTodoInput =
    { title : OptionalArgument String
    , completed : OptionalArgument Bool
    }


{-| Encode a UpdateTodoInput into a value that can be used as an argument.
-}
encodeUpdateTodoInput : UpdateTodoInput -> Value
encodeUpdateTodoInput input____ =
    Encode.maybeObject
        [ ( "title", Encode.string |> Encode.optional input____.title ), ( "completed", Encode.bool |> Encode.optional input____.completed ) ]


buildUpdateUserInput :
    (UpdateUserInputOptionalFields -> UpdateUserInputOptionalFields)
    -> UpdateUserInput
buildUpdateUserInput fillOptionals____ =
    let
        optionals____ =
            fillOptionals____
                { name = Absent, username = Absent, email = Absent, address = Absent, phone = Absent, website = Absent, company = Absent }
    in
    { name = optionals____.name, username = optionals____.username, email = optionals____.email, address = optionals____.address, phone = optionals____.phone, website = optionals____.website, company = optionals____.company }


type alias UpdateUserInputOptionalFields =
    { name : OptionalArgument String
    , username : OptionalArgument String
    , email : OptionalArgument String
    , address : OptionalArgument AddressInput
    , phone : OptionalArgument String
    , website : OptionalArgument String
    , company : OptionalArgument CompanyInput
    }


{-| Type for the UpdateUserInput input object.
-}
type alias UpdateUserInput =
    { name : OptionalArgument String
    , username : OptionalArgument String
    , email : OptionalArgument String
    , address : OptionalArgument AddressInput
    , phone : OptionalArgument String
    , website : OptionalArgument String
    , company : OptionalArgument CompanyInput
    }


{-| Encode a UpdateUserInput into a value that can be used as an argument.
-}
encodeUpdateUserInput : UpdateUserInput -> Value
encodeUpdateUserInput input____ =
    Encode.maybeObject
        [ ( "name", Encode.string |> Encode.optional input____.name ), ( "username", Encode.string |> Encode.optional input____.username ), ( "email", Encode.string |> Encode.optional input____.email ), ( "address", encodeAddressInput |> Encode.optional input____.address ), ( "phone", Encode.string |> Encode.optional input____.phone ), ( "website", Encode.string |> Encode.optional input____.website ), ( "company", encodeCompanyInput |> Encode.optional input____.company ) ]