module Main exposing (Msg(..), main)

import Browser
import Form exposing (Form)
import Form.Input
import Form.Validate as Validate exposing (Validation)
import Graphql.Http exposing (Request)
import Graphql.Operation exposing (RootMutation, RootQuery)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events
import PlaceholderApi.InputObject
import PlaceholderApi.Mutation
import PlaceholderApi.Object.User as User
import PlaceholderApi.Object.UsersPage as UsersPage
import PlaceholderApi.Query as Query



-- API


query : SelectionSet a RootQuery -> Request a
query selectionSet =
    Graphql.Http.queryRequest "https://graphqlzero.almansi.me/api" selectionSet


mutation : SelectionSet a RootMutation -> Request a
mutation selectionSet =
    Graphql.Http.mutationRequest "https://graphqlzero.almansi.me/api" selectionSet



-- MODEL


type alias Model =
    { users : List String
    , form : Form () PlaceholderApi.InputObject.CreateUserInput
    }


initialModel : Model
initialModel =
    { users = []
    , form = Form.initial [] validate
    }



-- FORM


optionalAbsent : Validation e a -> Validation e (Graphql.OptionalArgument.OptionalArgument a)
optionalAbsent =
    Validate.maybe >> Validate.map Graphql.OptionalArgument.fromMaybe


optionalNull : Validation e a -> Validation e (Graphql.OptionalArgument.OptionalArgument a)
optionalNull =
    Validate.maybe >> Validate.map Graphql.OptionalArgument.fromMaybeWithNull


validate : Validation () PlaceholderApi.InputObject.CreateUserInput
validate =
    let
        validateGeo =
            Validate.succeed PlaceholderApi.InputObject.GeoInput
                |> Validate.andMap (Validate.field "lat" Validate.float |> optionalAbsent)
                |> Validate.andMap (Validate.field "lng" Validate.float |> optionalAbsent)

        validateAddress =
            Validate.succeed PlaceholderApi.InputObject.AddressInput
                |> Validate.andMap (Validate.field "street" Validate.string |> optionalAbsent)
                |> Validate.andMap (Validate.field "suite" Validate.string |> optionalAbsent)
                |> Validate.andMap (Validate.field "city" Validate.string |> optionalAbsent)
                |> Validate.andMap (Validate.field "zipcode" Validate.string |> optionalAbsent)
                |> Validate.andMap (validateGeo |> optionalAbsent)

        validateCompany =
            Validate.succeed PlaceholderApi.InputObject.CompanyInput
                |> Validate.andMap (Validate.field "companyName" Validate.string |> optionalAbsent)
                |> Validate.andMap (Validate.field "catchPhrase" Validate.string |> optionalAbsent)
                |> Validate.andMap (Validate.field "bs" Validate.string |> optionalAbsent)

        requiredFields =
            Validate.succeed PlaceholderApi.InputObject.CreateUserInputRequiredFields
                |> Validate.andMap (Validate.field "name" Validate.string)
                |> Validate.andMap (Validate.field "username" Validate.string)
                |> Validate.andMap (Validate.field "email" Validate.email)

        optionalFields =
            Validate.succeed PlaceholderApi.InputObject.CreateUserInputOptionalFields
                |> Validate.andMap (validateAddress |> optionalAbsent)
                |> Validate.andMap (Validate.field "phone" Validate.string |> optionalAbsent)
                |> Validate.andMap (Validate.field "website" Validate.string |> optionalAbsent)
                |> Validate.andMap (validateCompany |> optionalAbsent)
                |> Validate.map always
    in
    Validate.succeed PlaceholderApi.InputObject.buildCreateUserInput
        |> Validate.andMap requiredFields
        |> Validate.andMap optionalFields



-- LIFECYCLE


type Msg
    = QueryResponse (Result (Graphql.Http.Error (List String)) (List String))
    | FormMsg Form.Msg
    | FormSubmited
    | CreatedUser (Result (Graphql.Http.Error String) String)


init : ( Model, Cmd Msg )
init =
    let
        userSelection =
            User.name
                |> SelectionSet.nonNullOrFail

        usersPageSelection =
            UsersPage.data userSelection
                |> SelectionSet.nonNullOrFail
                |> SelectionSet.nonNullElementsOrFail

        usersQuery =
            Query.users (\a -> always a ()) usersPageSelection
                |> SelectionSet.map (Maybe.withDefault [])
    in
    ( initialModel
    , Graphql.Http.send QueryResponse (query usersQuery)
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        QueryResponse (Ok users) ->
            ( { model | users = users }, Cmd.none )

        QueryResponse (Err _) ->
            ( model, Cmd.none )

        FormMsg subMsg ->
            ( { model | form = Form.update validate subMsg model.form }, Cmd.none )

        FormSubmited ->
            let
                validated =
                    ( Form.getOutput model.form, Form.getErrors model.form )
                        |> Debug.log "FORM DATA"

                userSelection =
                    User.name |> SelectionSet.nonNullOrFail

                createUserMutation input =
                    PlaceholderApi.Mutation.createUser { input = input } userSelection
                        |> SelectionSet.nonNullOrFail

                cmd =
                    Form.getOutput model.form
                        |> Maybe.map (createUserMutation >> mutation >> Graphql.Http.send CreatedUser)
                        |> Maybe.withDefault Cmd.none
            in
            ( { model | form = Form.update validate Form.Submit model.form }, cmd )

        CreatedUser response ->
            let
                log =
                    Debug.log "RESPONSE" response
            in
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    Html.div [ Attributes.class "flex flex-col items-center space-y-8" ]
        [ Html.h1 [ Attributes.class "text-4xl font-bold text-blue-500" ] [ Html.text "Vite and Elm starter" ]
        , Html.div [ Attributes.class "flex flex-col max-w-xl w-full" ] <|
            List.map (\user -> Html.p [] [ Html.text user ]) model.users
        , viewForm model.form
        ]


viewForm : Form () PlaceholderApi.InputObject.CreateUserInput -> Html Msg
viewForm form =
    let
        getTextField s =
            Form.getFieldAsString s form
    in
    Html.form [ Attributes.class "max-w-xl w-full space-y-4", Html.Events.onSubmit FormSubmited ]
        [ viewTextField "Name" (getTextField "name")
        , viewTextField "Username" (getTextField "username")
        , viewTextField "Email" (getTextField "email")
        , Html.input
            [ Attributes.type_ "submit"
            , Attributes.value "Submit"
            , Attributes.class "bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded focus:outline-none focus:shadow-outline hover:cursor-pointer"
            ]
            []
        ]


viewTextField : String -> Form.FieldState e String -> Html Msg
viewTextField label field =
    Html.div [ Attributes.class "space-y-2" ]
        [ Html.label
            [ Attributes.for field.path
            , Attributes.class "block text-gray-700 text-sm font-bold"
            ]
            [ Html.text label ]
        , Form.Input.textInput field
            [ Attributes.id field.path
            , Attributes.class "shadow appearance-none border rounded w-full py-2 px-3 text-gray-700 leading-tight focus:outline-none focus:shadow-outline"
            , Attributes.placeholder label
            ]
        , errorFor field
        ]
        |> Html.map FormMsg


errorFor : Form.FieldState e String -> Html msg
errorFor field =
    case field.liveError of
        Just error ->
            -- replace toString with your own translations
            Html.div [ Attributes.class "text-red-600" ]
                [ Html.text (Debug.toString error) ]

        Nothing ->
            Html.text ""


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> init
        , update = update
        , view =
            \model ->
                { title = "elm-template"
                , body = [ view model ]
                }
        , subscriptions = \_ -> Sub.none
        }
