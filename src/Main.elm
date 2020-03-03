module Main exposing (..)

import Dict
import List exposing (range)


import Browser
import Html exposing (Html, button, div, text, table, tr, td)
import Html.Events exposing (onClick)

main =
  Browser.sandbox { init = init, update = update, view = view }

type Msg = NoOp | PlayMove Int Int

type Player = Player1 | Player2

playerName player =
    case player of
        Player1 -> "X"
        Player2 -> "O"

init = {
        currentPlayer = Player1
        , moves = []
        , cells = Dict.empty
    }

update msg model =
    case msg of
        NoOp -> model
        PlayMove x y -> if Dict.member (x ,y) model.cells
            then
                model
            else
                { model |
                currentPlayer = case model.currentPlayer of
                    Player1 -> Player2
                    Player2 -> Player1
                , moves = (PlayMove x y) :: model.moves
                , cells = Dict.insert (x, y) model.currentPlayer model.cells
                }

aButton model x y =
    let
        cellText = case (Dict.get (x, y) model.cells) of
            Nothing -> "."
            Just Player1 -> "X"
            Just Player2 -> "O"
    in
    button [ onClick (PlayMove x y) ] [ text (cellText) ]

mkTable : (a -> b -> Html msg) -> List a -> List b -> Html msg
mkTable mkData rows columns =
    let
        mkCell a x = [mkData a x]
        mkRow a = List.map (td [] << mkCell a) columns
        data = List.map (tr [] << mkRow) rows
    in
    table [] data

view model =
    div [] [
        text (playerName model.currentPlayer)
        , mkTable (aButton model) (range 0 2) (range 0 2)
    ]

pair : x -> y -> (x , y)
pair x y = (x , y)

cartesian : (a -> b -> c) -> List a -> List b -> List c
cartesian f la lb = la |> List.concatMap (\x -> List.map (f x) lb)

validMoves model =
    let
        played pos = Dict.member pos model.cells
        allPositions = cartesian pair (range 0 2) (range 0 2)
    in List.filter (not << played) allPositions
