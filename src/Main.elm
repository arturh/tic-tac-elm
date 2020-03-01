module Main exposing (..)

import Dict

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
        PlayMove x y -> { 
            model |
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

view model =
    div [] [
        text (playerName model.currentPlayer)
        , table [] [
            tr [] [
                td [] [aButton model 0 0]
                , td [] [aButton model 0 1]
                , td [] [aButton model 0 2]
            ],
            tr [] [
                td [] [aButton model 1 0]
                , td [] [aButton model 1 1]
                , td [] [aButton model 1 2]
            ],
            tr [] [
                td [] [aButton model 2 0]
                , td [] [aButton model 2 1]
                , td [] [aButton model 2 2]
            ]
        ]
    ]
  