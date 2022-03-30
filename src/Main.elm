module Main exposing (..)

import Browser
import Html exposing (Html, button, div, img, text)
import Html.Attributes exposing (class, classList, src)
import Html.Events exposing (onClick)


type Stage
    = Start
    | Playing
    | Outcome


type Gesture
    = Rock
    | Paper
    | Scissors
    | Lizard
    | Spock


type Winner
    = Draw
    | User
    | Computer


stageToString : Stage -> String
stageToString stage =
    case stage of
        Start ->
            "Start"

        Playing ->
            "Playing"

        Outcome ->
            "Outcome"


gestureToString : Maybe Gesture -> String
gestureToString maybeGesture =
    case maybeGesture of
        Just Rock ->
            "Rock"

        Just Paper ->
            "Paper"

        Just Scissors ->
            "Scissors"

        Just Lizard ->
            "Lizard"

        Just Spock ->
            "Spock"

        Nothing ->
            "No gesture yet"


winnerToString : Winner -> String
winnerToString result =
    case result of
        Draw ->
            "It's a draw - let's try again"

        User ->
            "You win"

        Computer ->
            "You lost"


winner : Model -> Winner
winner model =
    case (model.userGesture, model.computerGesture) of
        (Just ug, Just cg) ->
            if ug == cg then
                Draw

            else
                case ( ug, cg ) of
                    ( Rock, Scissors ) ->
                        User

                    ( Rock, Lizard ) ->
                        User

                    ( Paper, Rock ) ->
                        User

                    ( Paper, Spock ) ->
                        User

                    ( Scissors, Paper ) ->
                        User

                    ( Scissors, Lizard ) ->
                        User

                    ( Lizard, Paper ) ->
                        User

                    ( Lizard, Spock ) ->
                        User

                    ( Spock, Rock ) ->
                        User

                    ( Spock, Scissors ) ->
                        User

                    ( _, _ ) ->
                        Computer

        (_, _) -> Draw

-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { stage : Stage
    , userGesture : Maybe Gesture
    , computerGesture : Maybe Gesture
    , userScore : Int
    , computerScore : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Start Nothing Nothing 0 0
    , Cmd.none
    )



-- UPDATE


type Msg
    = NoOp
    | GestureClicked Gesture


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GestureClicked gesture ->
            let
                newModel =
                    case model.stage of
                        Start ->
                            { model | userGesture = Just gesture, stage = Playing }

                        Playing ->
                            { model | computerGesture = Just gesture, stage = Outcome }

                        _ -> model
            in
            ( newModel, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div [] (viewHeader model ++ viewContent model ++ viewFooter model)



-- case model.userGesture of
--     Just gesture ->
--         text (gestureToString gesture)
--     Nothing ->
--         text "time to play"


viewHeader : Model -> List (Html msg)
viewHeader model =
    [ div [] [ text (stageToString model.stage) ] ]


viewContent : Model -> List (Html Msg)
viewContent model =
    case model.stage of
        Start ->
            viewStart model

        Playing ->
            viewPlaying model

        Outcome ->
            viewOutcome model


viewFooter : Model -> List (Html msg)
viewFooter model =
    [ div []
        [ text (stageToString model.stage)
        , text (gestureToString model.userGesture)
        , text (gestureToString model.computerGesture)
        , text (String.fromInt model.userScore)
        , text (String.fromInt model.computerScore)
        ]
    ]


viewStart : Model -> List (Html Msg)
viewStart model =
    [ div [] [ viewGestureWheel model ]
    ]


viewPlaying : Model -> List (Html Msg)
viewPlaying model =
    [ div [] [ viewGesture (Maybe.withDefault Rock model.userGesture) True ]
    , div [] [ viewGestureWheel model ]
    ]


viewOutcome : Model -> List (Html Msg)
viewOutcome model =
    [ viewGesture (Maybe.withDefault Rock model.userGesture) True
    , viewGesture (Maybe.withDefault Rock model.computerGesture) True
    , text <| winnerToString <| winner model
    , button [] [ text "Play again" ]
    ]


viewGesture : Gesture -> Bool -> Html Msg
viewGesture gesture selected =
    let
        source =
            case gesture of
                Rock ->
                    "assets/rock.png"

                Paper ->
                    "assets/paper.png"

                Scissors ->
                    "assets/scissors.png"

                Lizard ->
                    "assets/lizard.png"

                Spock ->
                    "assets/spock.png"
    in
    div [ class "gesture-container", classList [ ( "selected", selected ) ], onClick (GestureClicked gesture) ]
        [ img [ class "gesture", src source ] [] ]


viewGestureWheel : Model -> Html Msg
viewGestureWheel model =
    let
        attr =
            case model.stage of
                Start ->
                    "user-gesture"

                Playing ->
                    "computer-gesture"

                _ ->
                    ""

        selected gesture =
            case model.stage of
                Start ->
                    Just gesture == model.userGesture

                Playing ->
                    Just gesture == model.computerGesture

                _ ->
                    False
    in
    div [ class (String.join " " [ "gesture-wheel", attr ]) ]
        [ viewGesture Rock (selected Rock)
        , viewGesture Paper (selected Paper)
        , viewGesture Scissors (selected Scissors)
        , viewGesture Lizard (selected Lizard)
        , viewGesture Spock (selected Spock)
        ]



-- div [ class "gesture-wheel" ]
--     [ viewGesture Lizard ]
-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
