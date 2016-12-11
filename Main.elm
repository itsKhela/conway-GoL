module Main exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Array exposing (Array)


getNeighbors : Cell -> Board -> List Cell
getNeighbors (x,y) board =
  [ (x-1, y+1), (x, y+1), (x+1, y+1)
  , (x-1,   y),           (x+1,   y)
  , (x-1, y-1), (x, y-1), (x+1, y-1)
  ]

getCellState : Cell -> Board -> Bool
getCellState (x, y) board =
  -- if we are out of bounds = false
  -- else if there is cell a position = Value of position.
  case Array.get x board of
    Nothing -> False
    Just innerArray ->
        case Array.get y innerArray of
            Nothing -> False
            Just value -> value

getNumberAliveNeighbors : Cell -> Board -> Int
getNumberAliveNeighbors cell board =
  getNeighbors cell board
    |> List.filter (\a -> getCellState cell board)
    |> List.length

checkLifeNextTurn : Cell -> Board -> Bool
checkLifeNextTurn cell board =
  if (getCellState cell board) then
    ((getNumberAliveNeighbors cell board == 2) ||
      (getNumberAliveNeighbors cell board == 3))
  else
    (getNumberAliveNeighbors cell board == 3)

life : Cell -> Board -> Board
life (x, y) board =
  let
    existingRow =
      Array.get x board
        |> Maybe.withDefault (Array.repeat (Array.length board) False)
  in
    board
      |> Array.set x
        (existingRow
          |> Array.set y True)

nextGeneration : Board -> Board
nextGeneration board  =
  let
    neighbors cell row =
      row
        |> Array.indexedMap (\x _ -> checkLifeNextTurn ( cell, x ) board)
  in
    board
      |> Array.indexedMap neighbors

initBoard : Int -> Board
initBoard size =
  Array.repeat size (Array.repeat size False)


-- Model
type alias Board =
  Array (Array Bool)

type alias Cell = (Int, Int)

type alias Model =
  { board : Board
  -- , generation : Int
  -- , aliveCells : Int
  }

firstBoard = initBoard 10
startModel =
  { board = firstBoard
  -- , generation = 0
  -- , alivecells = 0
  }


-- Update
type Msg
  = Next Board
  | Play Board
  | Pause Board

update : Msg -> Board -> Board
update msg board =
  case msg of
    Next board -> nextGeneration board
    Play board -> nextGeneration board
    Pause board -> nextGeneration board


-- View

viewSquare : Bool -> Html Msg
viewSquare bool =
  let
    colour = if (bool == True) then "black" else "white"
  in td [ style [("height", "20px"),("width", "20px")
        ,("background-color","colour")]]
    []

viewInnerArray : Array Bool -> Html Msg
viewInnerArray array =
  tr [] <|
    List.map viewSquare (Array.toList array)

viewBoard : Board -> Html Msg
viewBoard model =
  table [] <|
    List.map viewInnerArray (Array.toList model)

view : Model -> Html Msg
view model =
  div []
      [h1 []
        [text ("Conway's Game of Life")]
      , button
          [ type_ "button"
          , onClick Next]
          [ text "Next"]

      , button
          [ type_ "button"
          , onClick Play]
          [ text "Play"]

      , button
          [ type_ "button"
          , onClick Pause]
          [ text "Pause"]
      ]

main : Program Never Model Msg
main =
  Html.beginnerProgram
  { model = startModel
  , view = view
  , update = update
  }
