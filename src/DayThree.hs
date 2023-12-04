module DayThree where

import qualified Data.Text as T
import Data.List
import Data.Char

type Value = Int
type Col = Int
type NumCols = Int
type Row = Int
type NumRows = Int
type Index = Int
type Span = Int

data Coord = Coord Row Col deriving (Eq, Show, Ord)
type Surround = [Coord]
type Positions = [Coord]

getIndex :: NumCols -> Coord -> Index
getIndex numCols (Coord row col) = row * numCols + col

getCoord :: NumCols -> Index -> Coord
getCoord numCols idx = let 
  (row, col) = divMod idx numCols
  in 
    Coord row col

getSurround :: NumCols -> NumRows -> Coord -> Span -> Surround
getSurround numCols numRows (Coord row col) span = let
  range = [col..col+span-1]
  up = map (\t -> Coord (row-1) t) range
  down = map (\t -> Coord (row+1) t) range
  left = map (\t -> Coord (t+row) (col-1)) [-1, 0, 1]
  right = map (\t -> Coord (t+row) (col+span)) [-1, 0, 1]
  in 
    filter (isValidCoord numCols numRows) (up ++ right ++ down ++ left)

data Number = Number Value Coord Span Positions deriving (Eq, Show, Ord)
getValue :: Number -> Value
getValue (Number value _ _ _) = value

data Input = Input T.Text NumCols NumRows deriving (Eq, Show, Ord)

readInput :: String -> Input
readInput str = let 
  text = T.pack str
  (Just numCols) = T.findIndex (=='\n') text
  cleanText = T.replace (T.pack "\n") (T.pack "") text
  numRows = (T.length cleanText) `div` numCols
  in 
    Input cleanText numCols numRows

isValidCoord :: NumCols -> NumRows -> Coord -> Bool
isValidCoord numCols numRows (Coord row col) = row>=0 && row<numRows && col>=0 && col<numCols

getNumbers :: Input -> [Number]
getNumbers input = getNumbersRec input 0 [] where
  getNumbersRec input@(Input text numCols numRows) pos numbers = let
    remainder = T.drop pos text in
      case T.findIndex isDigit remainder of
        Just idx -> let 
          initNumber = T.drop idx remainder
          (num, _) = T.span isDigit initNumber
          span = T.length num
          value = read . T.unpack $ num :: Int
          initIdx = pos + idx
          newPos = pos + idx + span 
          coord = getCoord numCols initIdx
          positions = map (getCoord numCols) [initIdx..(initIdx+span-1)]
          in
            getNumbersRec input newPos ((Number value coord span positions):numbers)
        Nothing -> numbers

surroundHasSymbol :: Input -> Surround -> Bool
surroundHasSymbol (Input text numCols numRows) surround = let
  surroundString = map ((T.index text) . (getIndex numCols)) surround
  in
    any (\c -> c/='.' && (not $ isDigit c)) surroundString

filterNumbers :: Input -> [Number] -> [Number]
filterNumbers input@(Input _ numCols numRows) numbers = let 
  f (Number _ coord span _) = surroundHasSymbol input (getSurround numCols numRows coord span)
  in 
    filter f numbers

data Gear = Gear Coord Number Number deriving (Eq, Show, Ord)
gearValue :: Gear -> Int
gearValue (Gear _ number1 number2) = (getValue number1) * (getValue number2)

getAdjacentNumbers :: NumCols -> NumRows -> Coord -> [Number] -> [Number]
getAdjacentNumbers numCols numRows coord numbers = let
  surroundCoord = getSurround numCols numRows coord 1
  filterSurround (Number _ _ _ positions) = any id $ map (\s -> elem s positions) surroundCoord
  filteredNumbers = filter filterSurround numbers
  in 
    filteredNumbers

getGears :: Input -> [Number] -> [Gear]
getGears input numbers = getGearsRec input numbers 0 [] where
  getGearsRec input@(Input text numCols numRows) numbers pos gears = let
    remainder = T.drop pos text in
      case T.findIndex (=='*') remainder of
        Just idx -> let 
          coord = getCoord numCols (idx+pos)
          adjacentNumbers = getAdjacentNumbers numCols numRows coord numbers
          newPos = idx+pos+1
          in 
            case length adjacentNumbers of
              2 -> getGearsRec input numbers newPos ((Gear coord (adjacentNumbers!!0) (adjacentNumbers!!1)):gears)
              otherwise -> getGearsRec input numbers newPos gears
        Nothing -> gears


dayThree :: Bool -> IO ()
dayThree test = do 
  let fileName = if test then "data/day3/test.txt" else "data/day3/input.txt"
  dayInput <- readFile fileName
  let input = readInput dayInput
  let numbers = getNumbers input
  let filteredNumbers = filterNumbers input numbers
  print (sum $ map getValue filteredNumbers)
  let gears = getGears input numbers 
  print gears
  print (sum $ map gearValue gears)