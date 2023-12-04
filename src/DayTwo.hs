module DayTwo where


import Data.Char
import Data.List
import qualified Data.Map as Map
import Data.List.Split (splitOn)
import Data.Maybe

data Color = Green | Blue | Red deriving (Eq, Read, Show, Ord)
readColor :: String -> Color
readColor = read . capFirst . (map toLower) where 
  capFirst [] = [] 
  capFirst (x:xs) = toUpper x : xs

type Colors = Map.Map Color Int
type LineId = Int

parseColors :: String -> Colors
parseColors str = let 
  colors = splitOn "," (strip str)
  in Map.fromList $ map parseColor colors where
    parseColor str = let
      parts = splitOn " " (strip str)
      number = read (head parts) :: Int
      color = readColor (parts!!1)
      in (color, number)

parseLine :: String -> (LineId, [Colors])
parseLine str = let
  parts = splitOn ":" str
  n = read (drop 5 (parts!!0))
  allColors = splitOn ";" (parts!!1)
  in (n, map parseColors allColors)

strip :: String -> String
strip = reverse . strip' . reverse . strip' where
  strip' [] = []
  strip' (x:xs) = if x == ' ' then (strip' xs) else x:xs

isValid :: Colors -> Colors -> Bool
isValid box draw = let 
  hasEnough = Map.mapWithKey (\k v -> (Map.findWithDefault 0 k box) >= v) draw
  in all id $ Map.elems hasEnough

isLineValid :: Colors -> String -> Maybe LineId
isLineValid box str = let
  (lineId, draws) = parseLine str
  in if all id $ map (isValid box) draws then Just lineId else Nothing

getPowerLine :: String -> Int
getPowerLine str = let
  (_, draws) = parseLine str
  minBox = foldr combine (parseColors "0 red, 0 blue, 0 green") draws where
    combine draw1 draw2 = Map.mapWithKey (\k v -> max (Map.findWithDefault 0 k draw1) v) draw2
  in foldr (*) 1 $ Map.elems minBox


dayTwo :: Bool -> IO ()
dayTwo test = do 
  let fileName = if test then "data/day2/test.txt" else "data/day2/input.txt"
  dayInput <- readFile fileName
  let box = parseColors "12 red, 13 green, 14 blue"
  let checkLines = map (isLineValid box) (lines dayInput)
  print (sum $ catMaybes checkLines)
  let powerLines = map getPowerLine (lines dayInput)
  print (sum powerLines)