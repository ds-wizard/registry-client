module Common.FormView exposing (form, input, submit, textarea)

import Form exposing (Form)
import Form.Error exposing (ErrorValue(..))
import Form.Input as Input
import Html exposing (Attribute, Html, button, div, label, p, text)
import Html.Attributes exposing (class, classList, placeholder, rows, type_)
import Html.Events exposing (onSubmit)
import Utils exposing (flip)


type alias FormConfig msg =
    { loading : Bool
    , error : Bool
    , submitMsg : msg
    }


type alias FieldConfig msg =
    { label : String
    , field : String
    , tagger : Form.Msg -> msg
    }


type alias SubmitConfig =
    { label : String
    }


form : FormConfig msg -> List (Html msg) -> Html msg
form config =
    Html.form
        [ class "ui form"
        , classList
            [ ( "loading", config.loading )
            , ( "error", config.error )
            ]
        , onSubmit config.submitMsg
        ]


input : FieldConfig msg -> Form e o -> Html msg
input config =
    field (flip Input.textInput [ placeholder config.label ]) config


textarea : FieldConfig msg -> Form e o -> Html msg
textarea =
    field (flip Input.textArea [ rows 3 ])


field : (Form.FieldState e String -> Html Form.Msg) -> FieldConfig msg -> Form e o -> Html msg
field inputFn config formData =
    let
        formField =
            Form.getFieldAsString config.field formData

        ( error, errorClass ) =
            getErrors formField config.label
    in
    div [ class <| "field " ++ errorClass ]
        [ label [] [ text config.label ]
        , inputFn formField |> Html.map config.tagger
        , error
        ]


getErrors : Form.FieldState e String -> String -> ( Html msg, String )
getErrors formField labelText =
    case formField.liveError of
        Just error ->
            ( div [ class "ui pointing red basic label" ] [ text (toReadable error labelText) ]
            , "error"
            )

        Nothing ->
            ( text "", "" )


toReadable : ErrorValue e -> String -> String
toReadable error labelText =
    case error of
        Empty ->
            labelText ++ " cannot be empty"

        InvalidString ->
            labelText ++ " cannot be empty"

        InvalidEmail ->
            "This is not a valid email"

        InvalidFloat ->
            "This is not a valid number"

        SmallerFloatThan n ->
            "This should not be less than " ++ String.fromFloat n

        GreaterFloatThan n ->
            "This should not be more than " ++ String.fromFloat n

        _ ->
            "Invalid value"


submit : SubmitConfig -> Html msg
submit config =
    button [ class "ui submit button" ]
        [ text config.label
        ]
