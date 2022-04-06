module Main exposing (..)

import Browser
import Delay
import Html exposing (Html, button, div, h1, img, li, p, text)
import Html.Attributes exposing (class, classList, src)
import Html.Events exposing (onClick)
import Html.Keyed as Keyed
import Random


type Stage
    = Start
    | UserSelected Gesture
    | ComputerHighlighting Gesture Gesture
    | Outcome Gesture Gesture


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

        UserSelected ug ->
            "Playing " ++ gestureToString ug

        ComputerHighlighting _ cg ->
            "Computer highlighting " ++ gestureToString cg

        Outcome ug cg ->
            "Outcome " ++ gestureToString ug ++ " " ++ gestureToString cg


gestureToString : Gesture -> String
gestureToString gesture =
    case gesture of
        Rock ->
            "Rock"

        Paper ->
            "Paper"

        Scissors ->
            "Scissors"

        Lizard ->
            "Lizard"

        Spock ->
            "Spock"


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
winner ug cg =
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
    , userScore : Int
    , computerScore : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Start 0 0
    , Cmd.none
    )



-- UPDATE


type Msg
    = PlayAgain
    | UserGestureClicked Gesture
    | ComputerGestureSelected Int
    | ComputerGestureHighlighted Int Int
    | ShowOutcome Gesture


intToGesture : Int -> Gesture
intToGesture n =
    case modBy 5 n of
        1 ->
            Rock

        2 ->
            Paper

        3 ->
            Scissors

        4 ->
            Lizard

        _ ->
            Spock


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UserGestureClicked gesture ->
            ( { model | stage = UserSelected gesture }
            , Random.generate ComputerGestureSelected (Random.int 1 5)
            )

        ComputerGestureSelected n ->
            -- let
            --     steps =
            --         List.range 2 (n + 4)
            --             |> List.map (\x -> Delay.after (200 * x) (ComputerGestureHighlighted <| cg x))
            -- in
            ( model, Delay.after 100 <| ComputerGestureHighlighted 1 (n + 5) )

        -- <| cg 1) :: steps ++ [ Delay.after (200 * (n + 6)) (ShowOutcome <| cg n) ]) )
        ComputerGestureHighlighted count n ->
            let
                userG =
                    case model.stage of
                        UserSelected ug ->
                            ug

                        ComputerHighlighting userg _ ->
                            userg

                        _ ->
                            Rock
            in
            if count < n then
                ( { model | stage = ComputerHighlighting userG <| intToGesture count }, Delay.after 200 (ComputerGestureHighlighted (count + 1) n) )

            else
                ( model, Delay.after 100 (ShowOutcome <| intToGesture n) )

        ShowOutcome cg ->
            case model.stage of
                ComputerHighlighting userG _ ->
                    let
                        theWinner =
                            winner userG cg

                        newUserScore =
                            model.userScore
                                + (if theWinner == User then
                                    1

                                   else
                                    0
                                  )

                        newComputerScore =
                            model.computerScore
                                + (if theWinner == Computer then
                                    1

                                   else
                                    0
                                  )
                    in
                    ( { model | stage = Outcome userG cg, userScore = newUserScore, computerScore = newComputerScore }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        PlayAgain ->
            ( { model | stage = Start }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        (viewHeader model ++ (viewContent model :: viewFooter model))


viewHeader : Model -> List (Html msg)
viewHeader model =
    let
        activeGame =
            model.userScore + model.computerScore > 0
    in
    if activeGame then
        [ h1 [] [ text "Rock, Paper, Scissors, Lizard, Spock" ]
        , p [] [ text "Your chance to play the iconic game from The Big Bang Theory." ]
        , p [] [ text "Select your gesture to start the game." ]
        , p []
            [ text
                ("Scores: You - "
                    ++ String.fromInt model.userScore
                    ++ "  Computer - "
                    ++ String.fromInt model.computerScore
                )
            ]
        ]

    else
        [ h1 [] [ text "Rock, Paper, Scissors, Lizard, Spock" ]
        , p [] [ text "Your chance to play the iconic game from The Big Bang Theory." ]
        , p [] [ text "Select your gesture to start the game." ]
        ]


viewContent : Model -> Html Msg
viewContent model =
    Keyed.node "ul"
        []
        (case model.stage of
            Start ->
                [ ( "user", viewUserGesture Nothing ) ]

            UserSelected gesture ->
                [ ( "user", viewUserGesture (Just gesture) )
                ]

            ComputerHighlighting userG computerG ->
                [ ( "user", viewUserGesture (Just userG) )
                , ( "computer", viewComputerGesture computerG Nothing )
                ]

            Outcome userG computerG ->
                [ ( "user", viewUserGesture (Just userG) )
                , ( "result", viewResult userG computerG )
                , ( "computer", viewComputerGesture computerG (Just computerG) )
                ]
        )


viewFooter : Model -> List (Html msg)
viewFooter model =
    [ div []
        [ text <| String.fromChar (Char.fromCode 169) ++ " Mark Farmiloe"
        , p [] [ text <| String.fromInt model.userScore ++ " " ++ String.fromInt model.computerScore ]

        -- , p [] [ text <| stageToString model.stage ]
        ]
    ]


viewUserGesture : Maybe Gesture -> Html Msg
viewUserGesture maybeGesture =
    viewGestureWheel Nothing maybeGesture


viewResult : Gesture -> Gesture -> Html Msg
viewResult userG computerG =
    div [ class "result" ]
        [ text <| winnerToString <| winner userG computerG
        , button [ onClick PlayAgain ] [ text "Play again" ]
        ]


viewComputerGesture : Gesture -> Maybe Gesture -> Html Msg
viewComputerGesture highlightedGesture maybeGesture =
    viewGestureWheel (Just highlightedGesture) maybeGesture


viewGesture : Gesture -> Bool -> Bool -> Bool -> Html Msg
viewGesture gesture selected highlighted clickable =
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
    if clickable then
        div [ class "gesture-container", classList [ ( "selected", selected ), ( "highlighted", highlighted ) ], onClick (UserGestureClicked gesture) ]
            [ img [ class "gesture", src source ] [] ]

    else
        div [ class "gesture-container", classList [ ( "selected", selected ), ( "highlighted", highlighted ) ] ]
            [ img [ class "gesture", src source ] [] ]


viewGestureWheel : Maybe Gesture -> Maybe Gesture -> Html Msg
viewGestureWheel maybeHighlightedGesture maybeGesture =
    let
        highlighted gesture =
            case maybeHighlightedGesture of
                Just aGesture ->
                    aGesture == gesture

                Nothing ->
                    False

        selected gesture =
            case maybeGesture of
                Just aGesture ->
                    aGesture == gesture

                Nothing ->
                    False

        clickable =
            maybeHighlightedGesture == Nothing && maybeGesture == Nothing

        classA =
            if maybeHighlightedGesture == Nothing then
                "user"

            else
                "computer"
    in
    li
        [ classList [ ( "gesture-wheel", True ), ( classA, True ), ( "selected", maybeGesture /= Nothing ) ] ]
        [ viewGesture Rock (selected Rock) (highlighted Rock) clickable
        , viewGesture Paper (selected Paper) (highlighted Paper) clickable
        , viewGesture Scissors (selected Scissors) (highlighted Scissors) clickable
        , viewGesture Lizard (selected Lizard) (highlighted Lizard) clickable
        , viewGesture Spock (selected Spock) (highlighted Spock) clickable
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
