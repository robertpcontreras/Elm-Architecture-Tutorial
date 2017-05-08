import Html exposing (..)
import Char exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)


main =
  Html.beginnerProgram
    { model = model
    , view = view
    , update = update
    }



-- MODEL


type alias Model =
  { name : String
  , password : String
  , passwordAgain : String
  , age : String
  , showValidation : String
  }


model : Model
model =
  Model "" "" "" "" "none"



-- UPDATE


type Msg
    = Name String
    | Password String
    | PasswordAgain String
    | Age String
    | ShowValidation


update : Msg -> Model -> Model
update msg model =
  case msg of
    Name name ->
      { model | name = name, showValidation = "none" }

    Password password ->
      { model | password = password, showValidation = "none" }

    PasswordAgain password ->
      { model | passwordAgain = password, showValidation = "none" }

    Age age ->
      { model | age = age, showValidation = "none" }

    ShowValidation ->
      { model | showValidation = "" }


-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ input [ type_ "text", placeholder "Name", onInput Name ] []
    , input [ type_ "password", placeholder "Password", onInput Password ] []
    , input [ type_ "password", placeholder "Re-enter Password", onInput PasswordAgain ] []
    , input [ type_ "text", placeholder "Age", onInput Age ] []
    , button [ onClick ShowValidation ] [ text "Show Validation" ]
    , div [ style [("display", model.showValidation)] ] [ viewValidation model ]
    ]


viewValidation : Model -> Html msg
viewValidation model =
  let
    (color, message) =
      if String.length model.password < 8 then
        ("red", "Password is too short. Must be longer than 8 digits!!")
      else if Result.withDefault -1 (String.toInt model.age) == -1 then
        ("red", "Your age is not a valid integer!")
      else if model.password == model.passwordAgain then
        ("green", "OK")
      else if String.any Char.isUpper model.password == False then
        ("red", "Your password must contain an uppercase character!")
      else if String.any Char.isLower model.password == False then
        ("red", "Your password must contain a lowercase character!")
      else
        ("red", "Passwords do not match!")
  in
    div [ style [("color", color)] ] [ text message ]

