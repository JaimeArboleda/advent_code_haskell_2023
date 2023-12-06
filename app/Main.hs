module Main where

import DayOne (dayOne)
import DayTwo (dayTwo)
import DayThree (dayThree)
import DayFour (dayFour)
import DayFive (dayFive)
import System.Environment

main :: IO ()
main = do
  a <- getArgs
  let day = head a
  let test = if (length a == 1) then False else (a!!1 == "test")
  case day of
    "day1" -> dayOne test
    "day2" -> dayTwo test
    "day3" -> dayThree test
    "day4" -> dayFour test
    "day5" -> dayFive test
    otherwise -> print "Not implemented"