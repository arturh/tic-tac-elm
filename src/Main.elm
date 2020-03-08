module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, button, div, table, td, text, tr)
import Html.Events exposing (onClick)
import List exposing (range)
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
    }


emptyModel : Model
emptyModel =
    { currentPlayer = Player1
    , previousMove = Newgame
    , cells = Dict.empty
    }


init : () -> ( Model, Cmd msg )
init () =
    ( emptyModel, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        PlayMove position ->
            updatePlayMove model position

        Random ->
            updateRandom model

        Undo ->
            case model.previousMove of
                Newgame ->
                    ( model, Cmd.none )

                Replay previous ->
                    ( previous, Cmd.none )

        Reset ->
            ( emptyModel, Cmd.none )


updateRandom : Model -> ( Model, Cmd Msg )
updateRandom model =
    case validMoves model of
        [] ->
            ( model, Cmd.none )

        m :: ms ->
            ( model, Random.generate PlayMove <| Random.uniform m ms )


updatePlayMove : Model -> Position -> ( Model, Cmd Msg )
updatePlayMove model position =
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

            newModel =
                { model
                    | currentPlayer = newCurrentPlayer
                    , previousMove = Replay model
                    , cells = newCells
                }
        in
        ( newModel, Cmd.none )


aButton : Model -> Position -> Html Msg
aButton model pos =
    let
        cellText =
            case Dict.get pos model.cells of
                Nothing ->
                    "."

                Just Player1 ->
                    "X"

                Just Player2 ->
                    "O"
    in
    button [ onClick (PlayMove pos) ] [ text cellText ]


viewTable : Model -> Html Msg
viewTable model =
    let
        mkCell i j =
            [ aButton model ( i, j ) ]

        mkRow i =
            List.map (td [] << mkCell i) rangeBoardSize

        trs =
            List.map (tr [] << mkRow) rangeBoardSize
    in
    table [] trs


view : Model -> Html Msg
view model =
    div []
        [ text (playerName model.currentPlayer)
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
        isPlayed pos =
            Dict.member pos model.cells
    in
    allMoves
        |> List.filter (not << isPlayed)


winning : List (List Position)
winning =
    [ [ ( 0, 0 ), ( 0, 1 ), ( 0, 2 ) ]
    , [ ( 1, 0 ), ( 2, 1 ), ( 1, 2 ) ]
    , [ ( 2, 0 ), ( 2, 1 ), ( 2, 2 ) ]
    , [ ( 0, 0 ), ( 1, 0 ), ( 2, 0 ) ]
    , [ ( 0, 1 ), ( 1, 1 ), ( 2, 1 ) ]
    , [ ( 0, 2 ), ( 1, 2 ), ( 2, 2 ) ]
    , [ ( 0, 0 ), ( 1, 1 ), ( 2, 2 ) ]
    , [ ( 0, 2 ), ( 1, 1 ), ( 2, 0 ) ]
    ]
