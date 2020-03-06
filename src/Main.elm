module Main exposing (..)

import Browser
import Dict
import Html exposing (Html, button, div, table, td, text, tr)
import Html.Events exposing (onClick)
import List exposing (range)
import Random
import Tuple exposing (pair)


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


noCmd x =
    ( x, Cmd.none )


type Msg
    = NoOp
    | PlayMove Int Int
    | Random
    | Reset


type Player
    = Player1
    | Player2


playerName player =
    case player of
        Player1 ->
            "X"

        Player2 ->
            "O"


emptyModel =
    { currentPlayer = Player1
    , moves = []
    , cells = Dict.empty
    }


init () =
    noCmd emptyModel


subscriptions _ =
    Sub.none


update msg model =
    case msg of
        NoOp ->
            noCmd model

        PlayMove x y ->
            if Dict.member ( x, y ) model.cells then
                noCmd model

            else
                noCmd
                    { model
                        | currentPlayer =
                            case model.currentPlayer of
                                Player1 ->
                                    Player2

                                Player2 ->
                                    Player1
                        , moves = PlayMove x y :: model.moves
                        , cells = Dict.insert ( x, y ) model.currentPlayer model.cells
                    }

        Random ->
            case validMoves model of
                [] ->
                    noCmd model

                m :: ms ->
                    ( model, Random.generate (uncurry PlayMove) <| Random.uniform m ms )

        Reset ->
            noCmd emptyModel


aButton model x y =
    let
        cellText =
            case Dict.get ( x, y ) model.cells of
                Nothing ->
                    "."

                Just Player1 ->
                    "X"

                Just Player2 ->
                    "O"
    in
    button [ onClick (PlayMove x y) ] [ text cellText ]


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
mkTable : (a -> b -> Html msg) -> List a -> List b -> Html msg
mkTable mkData rows columns =
    let
        mkCell a x =
            [ mkData a x ]

        mkRow a =
            List.map (td [] << mkCell a) columns

        data =
            List.map (tr [] << mkRow) rows
    in
    table [] data


view model =
    div []
        [ text (playerName model.currentPlayer)
        , mkTable (aButton model) (range 0 2) (range 0 2)
        , button [ onClick Random ] [ text "Random" ]
        , button [ onClick Reset ] [ text "Reset" ]
        ]


{-| Natural transformation from `a -> b -> c` to `( a, b ) -> c`.

    uncurry (\x y -> x) = Tuple.first
    uncurry (\x y -> y) = Tuple.second
    uncurry pair = id

-}
uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry f ( x, y ) =
    f x y


{-| Monoidal (i.e. Applicative) structure for the List Monad.

    cartesian _ [] _ = []
    cartesian _ _ [] = []
    cartesian f (a :: as) bs = (map (f a) bs) ++ (cartesian f as bs)

    cartesian pair (a1 :: ... :: an) (b1 :: ... :: bk) =
        [ (a1, b1), ... , (a1, bk), (a2, b1), ... , (an, b1), ... , (an , bk)]

-}
cartesian : (a -> b -> c) -> List a -> List b -> List c
cartesian f la lb =
    la |> List.concatMap (\x -> List.map (f x) lb)


validMoves model =
    let
        played pos =
            Dict.member pos model.cells

        allPositions =
            cartesian pair (range 0 2) (range 0 2)
    in
    List.filter (not << played) allPositions
