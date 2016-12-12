module Main exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Array exposing (Array)


getNeighbors : Cell -> Model -> List Cell
getNeighbors (x,y) board =
  [ (x-1, y+1), (x, y+1), (x+1, y+1)
  , (x-1,   y),           (x+1,   y)
  , (x-1, y-1), (x, y-1), (x+1, y-1)
  ]

getCellState : Cell -> Model -> Bool
getCellState (x, y) board =
  -- if we are out of bounds = false
  -- else if there is cell a position = Value of position.
  case Array.get x board of
    Nothing -> False
    Just innerArray ->
        case Array.get y innerArray of
            Nothing -> False
            Just value -> value

getNumberAliveNeighbors : Cell -> Model -> Int
getNumberAliveNeighbors cell board =
  getNeighbors cell board
    |> List.filter (\a -> getCellState a board)
    |> List.length

checkLifeNextTurn : Cell -> Model -> Bool
checkLifeNextTurn cell board =
  if (getCellState cell board) then
    ((getNumberAliveNeighbors cell board == 2) ||
      (getNumberAliveNeighbors cell board == 3))
  else
    (getNumberAliveNeighbors cell board == 3)

life : Cell -> Model -> Model
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

nextGeneration : Model -> Model
nextGeneration board  =
  let
    neighbors cell row =
      row
        |> Array.indexedMap (\x _ -> checkLifeNextTurn ( cell, x ) board)
  in
    board
      |> Array.indexedMap neighbors

initModel : Int -> Model
initModel size =
  Array.repeat size (Array.repeat size False)


-- Model
type alias Model =
  Array (Array Bool)

type alias Cell = (Int, Int)


firstModel = initModel 10
listCell =
  [(2,1),(2,2),(2,3)]

initLife : List Cell -> Model -> Model
initLife listCell model =
  List.foldl (\ c m -> (life c m)) model listCell



-- Update
type Msg
  = Next Model
  | Play Model
  | Pause Model

update : Msg -> Model -> Model
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
        ,("background-color",colour)]]
    []

viewInnerArray : Array Bool -> Html Msg
viewInnerArray array =
  tr [] <|
    List.map viewSquare (Array.toList array)

viewModel : Model -> Html Msg
viewModel model =
  Html.table [] <|
    List.map viewInnerArray (Array.toList model)

view : Model -> Html Msg
view model =
  div []
      [h1 []
        [text ("Conway's Game of Life")]
      , button
          [ type_ "button"
          , onClick (Next model)]
          [ text "Next"]

      , button
          [ type_ "button"
          , onClick (Play model)]
          [ text "Play"]

      , button
          [ type_ "button"
          , onClick (Pause model)]
          [ text "Pause"]
      , viewModel model
      ]

main : Program Never Model Msg
main =
  Html.beginnerProgram
  { model = (initLife listCell firstModel)
  , view = view
  , update = update
  }
