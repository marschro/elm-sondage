module Sondage
    exposing
        ( Catalogue
        , Msg
        , Question
            ( Options
            , FreeText
            , Scale
            , TextBlock
            )
        , initCatalogue
        , isCatalogueCompleted
        , resolveCatalogueAsJson
        , update
        , view
        )

{-|
Sondage is a small module that helps building quesionnaires (Sondage in french).

It brings Types, functions and the whole view for displaying the questionnaire.


## What does it do?
Lorem ipsum dolor sit amet


## How to integrate it in the Elm-Architecture
Lorem ipsum dolor sit amet


## Types
@docs Catalogue, Msg, Question


## Functions
@docs initCatalogue, update, view, isCatalogueCompleted, resolveCatalogueAsJson
-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Encode as JE


{-| -}
type Question
    = Options
        { question : String
        , options : List String
        , answer : Maybe String
        }
    | FreeText
        { question : String
        , answer : Maybe String
        }
    | Scale
        { question : String
        , range : ( Int, Int )
        , answer : Maybe String
        }
    | TextBlock
        { heading : String
        , text : String
        }


{-| -}
type Catalogue
    = Catalogue
        { previous : List Question
        , current : Question
        , remaining : List Question
        , completed : Bool
        }


{-| -}
type Msg
    = Forwards Catalogue
    | Backwards Catalogue
    | Answer Catalogue String
    | Finish Catalogue


{-| -}
update : Msg -> Catalogue
update msg =
    case msg of
        Forwards catalogue ->
            forwards catalogue

        Backwards catalogue ->
            backwards catalogue

        Answer catalogue answer ->
            updateCatalogueWithAnswer catalogue answer

        Finish catalogue ->
            completeCatalogue catalogue


completeCatalogue : Catalogue -> Catalogue
completeCatalogue (Catalogue { previous, current, remaining, completed }) =
    Catalogue
        { previous = previous
        , current = current
        , remaining = remaining
        , completed = True
        }


{-| -}
isCatalogueCompleted : Catalogue -> Bool
isCatalogueCompleted (Catalogue { previous, current, remaining, completed }) =
    completed


{-| -}
initCatalogue : List Question -> Catalogue
initCatalogue questions =
    let
        dummy =
            Options
                { question = "Dummy"
                , options = [ "Yes", "No" ]
                , answer = Nothing
                }

        first =
            Maybe.withDefault dummy (List.head questions)

        rest =
            Maybe.withDefault [] (List.tail questions)
    in
        Catalogue
            { previous = []
            , current = first
            , remaining = rest
            , completed = False
            }


updateCatalogueWithAnswer : Catalogue -> String -> Catalogue
updateCatalogueWithAnswer (Catalogue { previous, current, remaining }) answer =
    let
        updated =
            case current of
                Options record ->
                    Options { record | answer = Just answer }

                FreeText record ->
                    FreeText { record | answer = Just answer }

                Scale record ->
                    Scale { record | answer = Just answer }

                TextBlock record ->
                    TextBlock record
    in
        Catalogue
            { previous = previous
            , current = updated
            , remaining = remaining
            , completed = False
            }


{-| -}
resolveCatalogueAsJson : Catalogue -> JE.Value
resolveCatalogueAsJson (Catalogue { previous, current, remaining }) =
    encodeListToJson (List.append previous (current :: remaining))


encodeListToJson : List Question -> JE.Value
encodeListToJson list =
    List.filterMap
        (\question ->
            case question of
                TextBlock record ->
                    Nothing

                Options record ->
                    Just
                        (JE.object
                            [ ( "type", JE.string "Options" )
                            , ( "question", JE.string record.question )
                            , ( "answer", JE.string (Maybe.withDefault "" record.answer) )
                            ]
                        )

                Scale record ->
                    Just
                        (JE.object
                            [ ( "type", JE.string "Scale" )
                            , ( "question", JE.string record.question )
                            , ( "answer", JE.string (Maybe.withDefault "" record.answer) )
                            ]
                        )

                FreeText record ->
                    Just
                        (JE.object
                            [ ( "type", JE.string "FreeText" )
                            , ( "question", JE.string record.question )
                            , ( "answer", JE.string (Maybe.withDefault "" record.answer) )
                            ]
                        )
        )
        list
        |> JE.list


forwards : Catalogue -> Catalogue
forwards (Catalogue { previous, current, remaining }) =
    let
        prev =
            if Maybe.withDefault current (List.head remaining) == current then
                previous
            else
                current :: previous
    in
        Catalogue
            { previous = prev
            , current = Maybe.withDefault current (List.head remaining)
            , remaining = Maybe.withDefault [] (List.tail remaining)
            , completed = False
            }


backwards : Catalogue -> Catalogue
backwards (Catalogue { previous, current, remaining }) =
    let
        remain =
            if Maybe.withDefault current (List.head previous) == current then
                remaining
            else
                current :: remaining
    in
        Catalogue
            { previous = Maybe.withDefault [] (List.tail previous)
            , current = Maybe.withDefault current (List.head previous)
            , remaining = remain
            , completed = False
            }


{-| -}
view : Catalogue -> Html Msg
view catalogue =
    case catalogue of
        Catalogue record ->
            case record.completed of
                True ->
                    div []
                        [ text "Finished :-)" ]

                False ->
                    div []
                        [ questionView catalogue
                        , backwardsButton catalogue
                        , forwardsButton catalogue
                        ]


forwardsButton : Catalogue -> Html Msg
forwardsButton catalogue =
    case catalogue of
        Catalogue { remaining } ->
            case remaining of
                [] ->
                    button [ onClick (Finish catalogue) ] [ text "Send" ]

                _ ->
                    button [ onClick (Forwards catalogue) ] [ text "next" ]


backwardsButton : Catalogue -> Html Msg
backwardsButton catalogue =
    case catalogue of
        Catalogue { previous } ->
            case previous of
                [] ->
                    text ""

                _ ->
                    button [ onClick (Backwards catalogue) ] [ text "previous" ]


questionView : Catalogue -> Html Msg
questionView catalogue =
    let
        current =
            case catalogue of
                Catalogue { current } ->
                    current
    in
        case current of
            Options data ->
                div []
                    [ h3 [] [ text data.question ]
                    , List.map
                        (\option ->
                            radioView
                                data.question
                                option
                                (Maybe.withDefault "" data.answer)
                                (Answer catalogue option)
                        )
                        data.options
                        |> fieldset []
                    ]

            FreeText data ->
                div []
                    [ h3 [] [ text data.question ]
                    , textareaView
                        data.answer
                        (Answer catalogue)
                    ]

            Scale data ->
                let
                    ( lowest, highest ) =
                        data.range
                in
                    div []
                        [ h3 [] [ text data.question ]
                        , List.map
                            (\i ->
                                radioView
                                    data.question
                                    (toString i)
                                    (Maybe.withDefault "" data.answer)
                                    (Answer catalogue (toString i))
                            )
                            (List.range lowest highest)
                            |> fieldset []
                        ]

            TextBlock data ->
                div []
                    [ h3 [] [ text data.heading ]
                    , p [] [ text data.text ]
                    ]


radioView : String -> String -> String -> msg -> Html msg
radioView namespace description answer msg =
    let
        base =
            [ type_ "radio"
            , name namespace
            , onClick msg
            , value description
            , id description
            ]

        attr =
            if description == answer then
                checked True :: base
            else
                base
    in
        span []
            [ label [ for description ] [ text description ]
            , input
                attr
                []
            ]


textareaView : Maybe String -> (String -> msg) -> Html msg
textareaView answer msg =
    case answer of
        Just answer ->
            textarea
                [ value answer
                , onInput msg
                ]
                []

        Nothing ->
            textarea
                [ onInput msg
                ]
                []
