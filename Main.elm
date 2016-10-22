module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as App
import Random
import Random.Array
import Array exposing (..)
import Array.Extra
import Time
import Process
import Task


-- model


type alias Board =
    Array (Array Bool)


type alias Model =
    { board : Board
    , expectedBoard : Board
    , gameOver : Bool
    , playerWon : Bool
    , turnCount : Int
    , isDisabled : Bool
    }


initModel : ( Model, Cmd Msg )
initModel =
    ( { board = generateEmptyMatrix 5
      , expectedBoard = Array.fromList []
      , gameOver = False
      , playerWon = False
      , turnCount = 0
      , isDisabled = True
      }
    , initializeGameBoard
    )


initializeGameBoard : Cmd Msg
initializeGameBoard =
    Random.generate NewBoard (squareMatrixGenerator 5 9 False)


generateEmptyMatrix : Int -> Array (Array Bool)
generateEmptyMatrix num =
    Array.repeat num (Array.repeat num False)


indexGenerator : Int -> Int -> Random.Generator (Array ( Int, Int ))
indexGenerator edgeSize sampleSize =
    List.map (\i -> List.map2 (,) (List.repeat edgeSize i) [0..(edgeSize - 1)]) [0..(edgeSize - 1)]
        |> List.concat
        |> Array.fromList
        |> Random.Array.shuffle
        |> Random.map (Array.slice 0 sampleSize)


squareMatrixGenerator : Int -> Int -> Bool -> Random.Generator (Array (Array Bool))
squareMatrixGenerator edgeSize sampleSize value =
    let
        initialMatrix =
            Array.repeat edgeSize (Array.repeat edgeSize value)

        invertPoint ( x, y ) =
            Array.Extra.update x (Array.Extra.update y not)

        indexes =
            (indexGenerator edgeSize sampleSize)
    in
        Random.map (Array.foldl invertPoint initialMatrix) indexes



-- update


type Msg
    = NoOp String
    | SelectTile Int Int
    | ShuffleBoard
    | NewBoard Board
    | AfterSleep ()


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case (Debug.log "msg" msg) of
        NoOp error ->
            ( model, Cmd.none )

        SelectTile idx idy ->
            selectTile model idx idy

        ShuffleBoard ->
            ( { model
                | turnCount = 0
                , playerWon = False
                , gameOver = False
                , isDisabled = True
              }
            , initializeGameBoard
            )

        NewBoard newBoard ->
            ( { model
                | board = newBoard
                , expectedBoard = newBoard
              }
            , Task.perform NoOp AfterSleep (Process.sleep (3 * Time.second))
            )

        AfterSleep _ ->
            ( { model
                | board = generateEmptyMatrix 5
                , gameOver = False
                , isDisabled = False
              }
            , Cmd.none
            )


selectTile : Model -> Int -> Int -> ( Model, Cmd Msg )
selectTile model idx idy =
    let
        isSelected =
            Maybe.withDefault False
                (Array.get
                    idx
                    (Maybe.withDefault Array.empty (Array.get idy model.board))
                )

        newTurnCount =
            if
                model.gameOver
                    || model.isDisabled
                    || isSelected
            then
                model.turnCount
            else
                model.turnCount + 1

        newBoard =
            if
                model.gameOver
                    || model.isDisabled
                    || isSelected
            then
                model.board
            else
                Array.set
                    idy
                    (Array.set
                        idx
                        True
                        (Maybe.withDefault (Array.fromList []) (Array.get idy model.board))
                    )
                    model.board

        newGameOver =
            if model.gameOver || model.isDisabled then
                model.gameOver
            else
                newTurnCount == 9

        newPlayerWon =
            if model.gameOver || model.isDisabled then
                model.playerWon
            else
                model.expectedBoard == newBoard
    in
        ( { model
            | turnCount = newTurnCount
            , board = newBoard
            , gameOver = newGameOver
            , playerWon = newPlayerWon
          }
        , Cmd.none
        )



-- view


view : Model -> Html Msg
view model =
    div [ class "scoreboard" ]
        [ h1 [] [ text "Recall" ]
        , h2 [] [ text "Select the nine tiles you see to win!" ]
        , header model
        , grid model
        ]


header : Model -> Html Msg
header model =
    div
        [ classList [ ( "is-hidden", not model.gameOver ) ] ]
        [ div [ classList [ ( "is-hidden", not model.playerWon ) ] ]
            [ text "You win!"
            , button [ onClick ShuffleBoard ] [ text "Do it again!" ]
            ]
        , div [ classList [ ( "is-hidden", model.playerWon ) ] ]
            [ text "Whomp... Sorry"
            , button [ onClick ShuffleBoard ] [ text "Try again..." ]
            ]
        ]


grid : Model -> Html Msg
grid model =
    div [ class "recall-grid" ]
        (Array.toList
            (Array.indexedMap
                (\idy row ->
                    div
                        [ class "recall-grid-row" ]
                        (Array.toList
                            (Array.indexedMap
                                (\idx tile ->
                                    div
                                        [ classList
                                            [ ( "recall-grid-tile", True )
                                            , ( "is-selected", tile )
                                            ]
                                        , onClick (SelectTile idx idy)
                                        ]
                                        []
                                )
                                row
                            )
                        )
                )
                model.board
            )
        )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Never
main =
    App.program
        { init = initModel
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
