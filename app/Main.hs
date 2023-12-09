module Main where

import DayOne (dayOne)
import DayTwo (dayTwo)
import DayThree (dayThree)
import DayFour (dayFour)
import DayFive (dayFive)
import DaySix (daySix)
import DaySeven (daySeven)
import DayEight (dayEight)
import DayNine (dayNine)
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
    "day6" -> daySix test
    "day7" -> daySeven test
    "day8" -> dayEight test
    "day9" -> dayNine test
    otherwise -> print "Not implemented"