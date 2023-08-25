module Main exposing (Msg(..), main)

import Browser
import Graphql.Http exposing (Request)
import Graphql.Operation exposing (RootQuery)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Html exposing (Html)
import Html.Attributes as Attributes
import PlaceholderApi.Object.User as User
import PlaceholderApi.Object.UsersPage as UsersPage
import PlaceholderApi.Query as Query



-- API


query : SelectionSet a RootQuery -> Request a
query selectionSet =
    Graphql.Http.queryRequest "https://graphqlzero.almansi.me/api" selectionSet



-- MODEL


type alias Model =
    { users : List String
    }



-- LIFECYCLE


type Msg
    = QueryResponse (Result (Graphql.Http.Error (Maybe (List String))) (Maybe (List String)))


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
    in
    ( { users = [] }, Graphql.Http.send QueryResponse (query usersQuery) )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        QueryResponse (Ok maybeUsers) ->
            ( { model | users = maybeUsers |> Maybe.withDefault [] }, Cmd.none )

        QueryResponse (Err _) ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    Html.div [ Attributes.class "flex flex-col items-center" ]
        [ Html.h1 [ Attributes.class "text-4xl font-bold text-blue-500" ] [ Html.text "Vite and Elm starter" ]
        , Html.div [ Attributes.class "flex flex-col items-center" ] <|
            List.map (\user -> Html.p [] [ Html.text user ]) model.users
        ]


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
