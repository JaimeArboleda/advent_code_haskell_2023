module Main where

import DayOne
import DayTwo
import System.Environment
import Data.Maybe

dayOne :: IO ()
dayOne = do 
  dayOneInput <- readFile "data/day1/input.txt"
  let fileLines = lines dayOneInput
  let calibrations1 = map (getCalibration digitsMap) fileLines
  let calibrations2 = map (getCalibration fullMap) fileLines
  print "First sum"
  print (sum calibrations1)
  print "Second sum"
  print (sum calibrations2)

dayTwo :: IO ()
dayTwo = do
  dayTwoInput <- readFile "data/day2/input.txt"
  let box = parseColors "12 red, 13 green, 14 blue"
  let checkLines = map (isLineValid box) (lines dayTwoInput)
  print (sum $ catMaybes checkLines)
  let powerLines = map getPowerLine (lines dayTwoInput)
  print (sum powerLines)

main :: IO ()
main = do
  a <- getArgs
  case head a of
    "day1" -> dayOne
    "day2" -> dayTwo
    otherwise -> print "Not implemented"