{-
   TODO: function that resolces with the catalogue as List
   TODO: funtine that resolves with the catalogue as json
-}


module Sondage
    exposing
        ( Catalogue(Catalogue)
        , Question(Options, FreeText, Scale)
        , Msg
        , initCatalogue
        , update
        , view
        )

{-|
This package provides an easy and simple way to create questionaires.
What does this package help you with?

# Make Sondage part of your Model
@docs Question

# Build the catalogue out of your questions
@docs Catalogue, Msg, initCatalogue, update, view
-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


{-|
    Documentation for Question Type
-}
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


{-|
    The Catalogue Type

-}
type Catalogue
    = Catalogue
        { previous : List Question
        , current : Question
        , remaining : List Question
        }


{-|
    Msg Type
-}
type Msg
    = Forwards Catalogue
    | Backwards Catalogue
    | Answer Catalogue String


{-|
    Update handles everything for you
-}
update : Msg -> Catalogue
update msg =
    case msg of
        Forwards catalogue ->
            forwards catalogue

        Backwards catalogue ->
            backwards catalogue

        Answer catalogue answer ->
            updateCatalogueWithAnswer catalogue answer


{-|
    Having defined your questions and answers, you have to initialise
    the List of Question as a Catalogue
-}
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
    in
        Catalogue
            { previous = previous
            , current = updated
            , remaining = remaining
            }


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
            }


{-|
    The view handles all the displying... thingy...
-}
view : Catalogue -> Html Msg
view catalogue =
    div []
        [ questionView catalogue
        , button [ onClick (Backwards catalogue) ] [ text "previous" ]
        , button [ onClick (Forwards catalogue) ] [ text "next" ]
        ]


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
