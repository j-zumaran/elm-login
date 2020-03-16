module Main exposing (..)

import Browser
import Browser.Navigation exposing (load)
import Html exposing (Html, Attribute, div, input, text, button)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)

import Http exposing (Body, Error(..))

import Json.Decode exposing (Decoder, field, int, string)



-- MAIN


main = Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }



-- MODEL

type alias User =
    { email: String
    , password: String
    }

type State
    = Init
    | Loading
    | Success String
    | Failure Http.Error

type alias Model =
    { state: State
    , user: User
    }

init : () -> (Model, Cmd Msg)
init _=
    (Model Init (User "juan@web.com" "asdasdasd"), Cmd.none)



-- UPDATE

type Msg
    = SubmitLogin
    | LoginResult (Result Http.Error String)
    | ChangeEmail String
    | ChangePassword String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SubmitLogin ->
            (set model Loading, login model.user)

        LoginResult res ->
            case res of
                Ok ok ->
                    (set model (Success ok), redirectHome)
                Err error ->
                    (set model (Failure error), Cmd.none)

        ChangeEmail email ->
            updateUser model (\user -> { user | email = email })

        ChangePassword password ->
            updateUser model (\user -> { user | password = password })


set: Model -> State -> Model
set model state =
    { model | state = state }

updateUser : Model -> (User -> User) -> (Model, Cmd Msg)
updateUser model transform =
    ({ model | user = transform model.user }, Cmd.none)


login: User -> Cmd Msg
login user =
    Http.request
        { body = formUrl user
        , expect = Http.expectString LoginResult
        , headers = []
        , method = "POST"
        , timeout = Nothing
        , tracker = Nothing
        , url = "http://localhost:8080/user/signin"
        }

formUrl : User -> Body
formUrl user =
    [ "email=" ++ user.email
    , "password=" ++ user.password
    ]
    |> String.join "&"
    |> Http.stringBody "application/x-www-form-urlencoded"

redirectHome: Cmd Msg
redirectHome =
    load "http://localhost:8080/user/home"

decodeResponse: Decoder String
decodeResponse =
    field "message" string

-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ div [] [ text ("Log in") ]
    , viewInput "text" "email" model.user.email ChangeEmail
    , viewInput "password" "password" model.user.password ChangePassword
    , div [] [ text ("Email: " ++ model.user.email ++ " password: " ++ model.user.password) ]
    , button [ onClick SubmitLogin ] [ text "Submit" ]
    , viewState model
    ]


viewInput : String -> String -> String -> (String -> Msg) -> Html Msg
viewInput tipo pHolder val toMsg =
  input [ type_ tipo, placeholder pHolder, value val, onInput toMsg ] []


viewState: Model -> Html Msg
viewState model =
    case model.state of
        Init ->
            div [] []
        Loading ->
            div [] [ text ("Loading...") ]
        Success s ->
            div [] [ text ("Succes " ++ s)]
        Failure error ->
            div [] [ text (errorToString error) ]


errorToString : Http.Error -> String
errorToString error =
    case error of
        BadUrl url ->
            "The URL " ++ url ++ " was invalid"
        Timeout ->
            "Unable to reach the server, try again"
        NetworkError ->
            "Unable to reach the server, check your network connection"
        BadStatus 500 ->
            "The server had a problem, try again later"
        BadStatus 400 ->
            "Verify your information and try again"
        BadStatus 401 ->
            "Unauthorized. Verify your information and try again"
        BadStatus status ->
            "Unknown error" ++ String.fromInt status
        BadBody errorMessage ->
            "bad body" ++ errorMessage


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none
