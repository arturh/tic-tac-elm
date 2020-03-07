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
    | Replay Msg Model


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
                        { model
                            | currentPlayer =
                                case model.currentPlayer of
                                    Player1 ->
                                        Player2

                                    Player2 ->
                                        Player1
                            , previousMove = Replay msg model
                            , cells = Dict.insert pos model.currentPlayer model.cells
                        }
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

                Replay _ previous ->
                    ( previous, Cmd.none )

        Reset ->
            ( emptyModel, Cmd.none )


aButton : Model -> ( Int, Int ) -> Html Msg
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
mkTable : (( a, b ) -> Html msg) -> List a -> List b -> Html msg
mkTable mkData rows columns =
    let
        mkCell a x =
            [ mkData ( a, x ) ]

        mkRow a =
            List.map (td [] << mkCell a) columns

        trs =
            List.map (tr [] << mkRow) rows
    in
    table [] trs


view : Model -> Html Msg
view model =
    div []
        [ text (playerName model.currentPlayer)
        , mkTable (aButton model) rangeBoardSize rangeBoardSize
        , button [ onClick Random ] [ text "Random" ]
        , button [ onClick Undo ] [ text "Undo" ]
        , button [ onClick Reset ] [ text "Reset" ]
        ]

boardSize: Int
boardSize = 5

rangeBoardSize: List Int
rangeBoardSize =
    range 0 (boardSize - 1)

cartesian : List a -> List b -> List ( a, b )
cartesian xs ys =
    List.concatMap
        (\x -> List.map (\y -> ( x, y )) ys)
        xs


allMoves : List ( Int, Int )
allMoves =
    cartesian rangeBoardSize rangeBoardSize


validMoves : Model -> List ( Int, Int )
validMoves model =
    let
        isPlayed pos =
            Dict.member pos model.cells
    in
    List.filter (not << isPlayed) allMoves
