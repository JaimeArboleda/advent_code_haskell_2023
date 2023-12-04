module DayOne where

import Control.Monad
import Control.Monad.Trans.State
import Data.Char
import Data.List
import qualified Data.Map as Map
import Control.Applicative (liftA2)

data ParseState = ParseState (Map.Map String Int) (Map.Map String Int) [Int] deriving (Show)

digits = map show [1..9]
englishNumbers = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
englishMap :: Map.Map String Int
englishMap = Map.fromList $ zip englishNumbers [1..9]
digitsMap :: Map.Map String Int
digitsMap = Map.fromList $ zip digits [1..9]
fullMap = Map.union englishMap digitsMap
buildInitialState :: Map.Map String Int -> ParseState 
buildInitialState map = ParseState map (Map.mapWithKey (\k v -> (-1)) map) []

nextState :: Char -> ParseState -> ParseState
nextState c (ParseState lookup readState numbers) = let
  nextLetters = Map.mapWithKey (\k v -> k!!(v+1)) readState
  interimReadState = Map.mapWithKey f readState where
    f k v = case Map.lookup k nextLetters of
      Just s -> if s == c then v+1 else
        if c == (head k) then 0 else (-1)
      Nothing -> -1
  numberFound k v = length k == (v+1) 
  nextReadState = Map.mapWithKey (\k v -> if numberFound k v then (-1) else v) interimReadState
  nextNumbers = numbers ++ (Map.elems $ Map.filterWithKey g lookup) where
    g k _ = let 
      mv = Map.lookup k interimReadState
      in case mv of
        Just v -> numberFound k v
        otherwise -> False
  in ParseState lookup nextReadState nextNumbers

updateAll :: String -> State ParseState ()
updateAll cs = mapM_ (\c -> modify (nextState c)) cs

getCalibration :: Map.Map String Int -> String -> Int
getCalibration map str = let 
    ParseState _ _ numbers = snd $ runState (updateAll str) (buildInitialState map)
    in (head numbers) * 10 + (last numbers)


dayOne :: Bool -> IO ()
dayOne test = do 
  let fileName = if test then "data/day1/test.txt" else "data/day1/input.txt"
  dayInput <- readFile fileName
  let fileLines = lines dayInput
  let calibrations1 = map (getCalibration digitsMap) fileLines
  let calibrations2 = map (getCalibration fullMap) fileLines
  print "First sum"
  print (sum calibrations1)
  print "Second sum"
  print (sum calibrations2)