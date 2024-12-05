module Main exposing (main)

import Browser
import Html exposing (Html, div, h1, text, textarea)
import Html.Attributes exposing (class, placeholder, value)
import Html.Events exposing (onInput)
import SyntaxHighlight exposing (monokai, toBlockHtml, useTheme)



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }



-- MODEL


type alias Model =
    { userInput : String }


init : Model
init =
    { userInput = "" }



-- UPDATE


type Msg
    = UpdateInput String


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateInput newInput ->
            { model | userInput = newInput }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Syntax Highlighting Demo" ]
        , useTheme monokai
        , textarea
            [ placeholder "Type your code here...\nExample:\n```python\ndef hello():\n    print('Hello!')\n```"
            , value model.userInput
            , onInput UpdateInput
            ]
            []
        , viewCodeWithHighlight model.userInput
        ]


viewCodeWithHighlight : String -> Html msg
viewCodeWithHighlight input =
    let
        -- Parse the input to extract language and code
        parts =
            String.split "```" input

        -- Check if we have valid format (```language\ncode```)
        maybeLanguageAndCode =
            case parts of
                [ "", languageAndCode, "" ] ->
                    case String.lines languageAndCode of
                        language :: codeLines ->
                            Just
                                { language = String.trim language
                                , code = String.join "\n" codeLines
                                }

                        [] ->
                            Nothing

                _ ->
                    Nothing
    in
    case maybeLanguageAndCode of
        Just { language, code } ->
            case language of
                "python" ->
                    code
                        |> SyntaxHighlight.python
                        |> Result.map (toBlockHtml (Just 1))
                        |> Result.withDefault (text code)

                "javascript" ->
                    code
                        |> SyntaxHighlight.javascript
                        |> Result.map (toBlockHtml (Just 1))
                        |> Result.withDefault (text code)

                "elm" ->
                    code
                        |> SyntaxHighlight.elm
                        |> Result.map (toBlockHtml (Just 1))
                        |> Result.withDefault (text code)

                _ ->
                    text "Unsupported language. Try 'python', 'javascript', or 'elm'"

        Nothing ->
            text "Format: ```language\\nyour code here```"
