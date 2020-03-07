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

        PlayMove pos ->
            if Dict.member pos model.cells then
                ( model, Cmd.none )

            else
                let
                    newModel =
                        playMove model pos
                in
                ( newModel, Cmd.none )

        Random ->
            case validMoves model of
                [] ->
                    ( model, Cmd.none )

                m :: ms ->
                    ( model, Random.generate PlayMove <| Random.uniform m ms )

        Undo ->
            case model.previousMove of
                Newgame ->
                    ( model, Cmd.none )

                Replay previous ->
                    ( previous, Cmd.none )

        Reset ->
            ( emptyModel, Cmd.none )


playMove : Model -> Position -> Model
playMove model pos =
    { model
        | currentPlayer =
            case model.currentPlayer of
                Player1 ->
                    Player2

                Player2 ->
                    Player1
        , previousMove = Replay model
        , cells = Dict.insert pos model.currentPlayer model.cells
    }


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


{-| Construct a table without `Attributes` crossing a `List a` and a `List b`
through a generating function (a -> b -> Html msg) to produce a table with
rows a and columns b.

    mkTable _ [] _ = table [] []
    mkTable _ (a1 :: ... :: an) [] = table [] (List.repeat n (tr [] []))
    mkTable f (a1 :: ... :: an) (b1 :: ... :: bk) = table [] [
        tr [] [
            td [] [f a1 b1],
            ...
            td [] [f a1 bk]
        ],
        ...
        tr [] [
            td [] [f an b1],
            ...
            td [] [f an bk]
        ],
    ]

-}
viewTable : Model -> Html Msg
viewTable model =
    let
            
        mkCell a x =
            [ aButton model ( a, x ) ]

        mkRow a =
            List.map (td [] << mkCell a) rangeBoardSize

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
    List.concatMap
        (\x -> List.map (pair x) ys)
        xs


allMoves : List Position
allMoves =
    cartesianPairs rangeBoardSize rangeBoardSize


validMoves : Model -> List Position
validMoves model =
    let
        isPlayed pos =
            Dict.member pos model.cells
    in
    List.filter (not << isPlayed) allMoves
