module Main where

import DayOne (getCalibration, digitsMap, fullMap)
import DayTwo (parseColors, isLineValid, getPowerLine)
import DayThree (readInput, getNumbers, filterNumbers, getValue, getGears, gearValue)
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

dayThree :: IO ()
dayThree = do
  dayThree <- readFile "data/day3/input.txt"
  let input = readInput dayThree
  let numbers = getNumbers input
  let filteredNumbers = filterNumbers input numbers
  print (sum $ map getValue filteredNumbers)
  let gears = getGears input numbers 
  print gears
  print (sum $ map gearValue gears)




main :: IO ()
main = do
  a <- getArgs
  case head a of
    "day1" -> dayOne
    "day2" -> dayTwo
    "day3" -> dayThree
    otherwise -> print "Not implemented"