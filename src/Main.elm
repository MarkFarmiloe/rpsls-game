module Main exposing (..)

import Browser
import Html exposing (Html, div, img, text)
import Html.Attributes exposing (class, classList, src)
import Html.Events exposing (onClick)


type Page
    = Home
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


pageToString : Page -> String
pageToString page =
    case page of
        Home ->
            "Home"

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


winner : Gesture -> Gesture -> Winner
winner user computer =
    if user == computer then
        Draw

    else
        case ( user, computer ) of
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
    { page : Page
    , userGesture : Maybe Gesture
    , computerGesture : Maybe Gesture
    , userScore : Int
    , computerScore : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Home Nothing Nothing 0 0
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
                    case model.userGesture of
                        Nothing ->
                            { model | userGesture = Just gesture }

                        _ ->
                            { model | computerGesture = Just gesture }
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


viewHeader model =
    [ div [] [ text "Header" ] ]


viewContent model =
    case model.page of
        Home ->
            viewHome model

        Playing ->
            viewPlaying model

        Outcome ->
            viewOutcome model


viewFooter : Model -> List (Html msg)
viewFooter model =
    [ div []
        [ text (pageToString model.page)
        , text (gestureToString model.userGesture)
        , text (gestureToString model.computerGesture)
        , text (String.fromInt model.userScore)
        , text (String.fromInt model.computerScore)
        ]
    ]


viewHome model =
    [ div [] [ viewGestureWheel model ]
    ]


viewPlaying model =
    [ text "Playing" ]


viewOutcome model =
    [ text "Outcome" ]


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


viewGestureWheel model =
    let
        attr =
            case model.page of
                Home ->
                    "user-gesture"

                Playing ->
                    "computer-gesture"

                _ ->
                    ""

        selected gesture =
            case model.page of
                Home ->
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
