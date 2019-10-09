module Main exposing (Model, Msg(..), main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import List exposing (..)



-- MODEL


main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    { name : String
    , password : String
    , passwordAgain : String
    }


init : Model
init =
    Model "" "" ""



-- UPDATE


type Msg
    = Name String
    | Password String
    | PasswordAgain String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Name name ->
            { model | name = name }

        Password password ->
            { model | password = password }

        PasswordAgain password ->
            { model | passwordAgain = password }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        ([ viewInput "text" "Name" model.name Name
         , viewInput "password" "Password" model.password Password
         , viewInput "password" "Re-enter Password" model.passwordAgain PasswordAgain
         ]
            ++ viewValidation model
        )


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []


viewValidation : Model -> List (Html msg)
viewValidation model =
    let
        errors =
            validateAll model
    in
    if List.isEmpty errors then
        [ div [ style "color" "green" ] [ text "Ok" ] ]

    else
        List.map (\error -> div [ style "color" "red" ] [ text error ]) errors


validateAll : Model -> List String
validateAll model =
    validatePasswordMatching model.password model.passwordAgain []
        |> validatePasswordLength8 model.password


validatePasswordLength8 : String -> List String -> List String
validatePasswordLength8 password errors =
    if String.length password >= 8 then
        errors

    else
        [ "Password is too short!" ] ++ errors


validatePasswordMatching : String -> String -> List String -> List String
validatePasswordMatching password passwordAgain errors =
    if password == passwordAgain then
        errors

    else
        [ "Password do not match!" ] ++ errors
