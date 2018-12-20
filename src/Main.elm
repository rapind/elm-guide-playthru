module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { name : String
    , age : Int
    , password : String
    , passwordAgain : String
    , submitted : Bool
    }


init : Model
init =
    Model "" 0 "" "" False



-- UPDATE


type Msg
    = ChangeName String
    | ChangeAge String
    | ChangePassword String
    | ChangePasswordAgain String
    | Submit


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeName name ->
            { model | name = name, submitted = False }

        ChangeAge age ->
            { model | age = Maybe.withDefault 0 (String.toInt age), submitted = False }

        ChangePassword password ->
            { model | password = password, submitted = False }

        ChangePasswordAgain password ->
            { model | passwordAgain = password, submitted = False }

        Submit ->
            { model | submitted = True }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewInput "text" "Name" model.name ChangeName
        , viewInput "text"
            "Age"
            (if model.age == 0 then
                ""

             else
                String.fromInt model.age
            )
            ChangeAge
        , viewInput "password" "Password" model.password ChangePassword
        , viewInput "password" "Re-enter Password" model.passwordAgain ChangePasswordAgain
        , input [ type_ "submit", onClick Submit ] [ text "Save" ]
        , viewValidation model
        ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []


viewValidation : Model -> Html msg
viewValidation model =
    if model.submitted == False then
        div [] []

    else if String.isEmpty model.name then
        div [ style "color" "red" ] [ text "Please enter a name." ]

    else if model.age == 0 then
        div [ style "color" "red" ] [ text "Please enter an age." ]

    else if model.age > 150 then
        div [ style "color" "red" ] [ text "Please enter a valid age." ]

    else if String.isEmpty model.password then
        div [ style "color" "red" ] [ text "Please enter a password." ]

    else if String.length model.password < 8 then
        div [ style "color" "red" ] [ text "Password is not long enough!" ]

    else if String.any Char.isDigit model.password == False then
        div [ style "color" "red" ] [ text "Password must contain at least one number." ]

    else if String.any Char.isUpper model.password == False then
        div [ style "color" "red" ] [ text "Password must contain at least one uppercase character." ]

    else if String.any Char.isLower model.password == False then
        div [ style "color" "red" ] [ text "Password must contain at least one lowercase character." ]

    else if model.password /= model.passwordAgain then
        div [ style "color" "red" ] [ text "Passwords do not match!" ]

    else
        div [ style "color" "green" ] [ text "OK" ]
