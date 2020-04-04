module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, button, div, table, td, text, tr)
import Html.Events exposing (onClick)
import List exposing (range)
import Maybe exposing (Maybe)
import Random
import Tuple exposing (pair)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type alias Position =
    ( Int, Int )


type Msg
    = NoOp
    | PlayMove Position
    | Random
    | Undo
    | Reset


type Player
    = Player1
    | Player2


playerName : Player -> String
playerName player =
    case player of
        Player1 ->
            "X"

        Player2 ->
            "O"


type Moves
    = Newgame
    | Replay Model


type alias Model =
    { currentPlayer : Player
    , previousMove : Moves
    , cells : Dict Position Player
    , counter : WinningCounter
    }


emptyModel : Model
emptyModel =
    { currentPlayer = Player1
    , previousMove = Newgame
    , cells = Dict.empty
    , counter = initCounter
    }


init : () -> ( Model, Cmd msg )
init () =
    ( emptyModel, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg =
    case msg of
        NoOp ->
            noOp

        PlayMove position ->
            updatePlayMove position

        Random ->
            updateRandom

        Undo ->
            undoMove

        Reset ->
            resetModel


noOp : Model -> ( Model, Cmd Msg )
noOp model =
    ( model, Cmd.none )


undoMove : Model -> ( Model, Cmd Msg )
undoMove model =
    case model.previousMove of
        Newgame ->
            ( model, Cmd.none )

        Replay previous ->
            ( previous, Cmd.none )


resetModel : Model -> ( Model, Cmd Msg )
resetModel _ =
    ( emptyModel, Cmd.none )


updateRandom : Model -> ( Model, Cmd Msg )
updateRandom model =
    case validMoves model of
        [] ->
            ( model, Cmd.none )

        m :: ms ->
            ( model, Random.generate PlayMove <| Random.uniform m ms )


updatePlayMove : Position -> Model -> ( Model, Cmd Msg )
updatePlayMove position model =
    if Dict.member position model.cells then
        ( model, Cmd.none )

    else
        let
            newCells =
                model.cells
                    |> Dict.insert position model.currentPlayer

            newCurrentPlayer =
                case model.currentPlayer of
                    Player1 ->
                        Player2

                    Player2 ->
                        Player1

            newCounter =
                updateCounter position model.currentPlayer model.counter

            newModel =
                { model
                    | currentPlayer = newCurrentPlayer
                    , previousMove = Replay model
                    , cells = newCells
                    , counter = newCounter
                }
        in
        ( newModel, Cmd.none )


aButton : Position -> Model -> Html Msg
aButton position model =
    let
        cellText =
            case Dict.get position model.cells of
                Nothing ->
                    "."

                Just Player1 ->
                    "X"

                Just Player2 ->
                    "O"
    in
    button [ onClick (PlayMove position) ] [ text cellText ]


viewTable : Model -> Html Msg
viewTable model =
    let
        mkCell i j =
            [ aButton ( i, j ) model ]

        mkRow i =
            List.map (td [] << mkCell i) rangeBoardSize

        trs =
            List.map (tr [] << mkRow) rangeBoardSize
    in
    table [] trs


viewWinner : Model -> Html Msg
viewWinner model =
    let
        winningMessage =
            case checkWinner model.counter of
                Nothing ->
                    playerName model.currentPlayer ++ " to play"

                Just player ->
                    playerName player ++ " wins"
    in
    text winningMessage


view : Model -> Html Msg
view model =
    div []
        [ viewWinner model
        , viewTable model
        , button [ onClick Random ] [ text "Random" ]
        , button [ onClick Undo ] [ text "Undo" ]
        , button [ onClick Reset ] [ text "Reset" ]
        ]


boardSize : Int
boardSize =
    3


rangeBoardSize : List Int
rangeBoardSize =
    range 0 (boardSize - 1)


cartesianPairs : List a -> List b -> List ( a, b )
cartesianPairs xs ys =
    xs
        |> List.concatMap
            (\x -> List.map (pair x) ys)


allMoves : List Position
allMoves =
    cartesianPairs rangeBoardSize rangeBoardSize


validMoves : Model -> List Position
validMoves model =
    let
        isPlayed position =
            Dict.member position model.cells
    in
    allMoves
        |> List.filter (not << isPlayed)


winning : List (List Position)
winning =
    [ [ ( 0, 0 ), ( 0, 1 ), ( 0, 2 ) ]
    , [ ( 1, 0 ), ( 1, 1 ), ( 1, 2 ) ]
    , [ ( 2, 0 ), ( 2, 1 ), ( 2, 2 ) ]
    , [ ( 0, 0 ), ( 1, 0 ), ( 2, 0 ) ]
    , [ ( 0, 1 ), ( 1, 1 ), ( 2, 1 ) ]
    , [ ( 0, 2 ), ( 1, 2 ), ( 2, 2 ) ]
    , [ ( 0, 0 ), ( 1, 1 ), ( 2, 2 ) ]
    , [ ( 0, 2 ), ( 1, 1 ), ( 2, 0 ) ]
    ]


type WinningStatus
    = Free
    | Lost
    | Win Player Int


type alias WinningCounter =
    Dict (List Position) WinningStatus


initCounter : WinningCounter
initCounter =
    winning |> List.map (\l -> ( l, Free )) |> Dict.fromList


updateCounter : Position -> Player -> WinningCounter -> WinningCounter
updateCounter position player =
    let
        updateWin winningStatus =
            case winningStatus of
                Free ->
                    Win player 1

                Lost ->
                    Lost

                Win occupant count ->
                    if player == occupant then
                        Win occupant (count + 1)

                    else
                        Lost

        updatePlayer win winningStatus =
            if List.member position win then
                updateWin winningStatus

            else
                winningStatus
    in
    Dict.map updatePlayer


checkWinner : WinningCounter -> Maybe Player
checkWinner =
    let
        checkWin _ winningStatus maybePlayer =
            case maybePlayer of
                Nothing ->
                    case winningStatus of
                        Win player counter ->
                            if counter == 3 then
                                Just player

                            else
                                Nothing

                        _ ->
                            Nothing

                justPlayer ->
                    justPlayer
    in
    Dict.foldr checkWin Nothing
