{-# LANGUAGE OverloadedStrings #-}
module DaySix where
import ParserUtils

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Void
import Data.List
import Data.Function
import Data.Maybe
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- Data

type Time = Int
type Speed = Int
type Hold = Int
type Distance = Int
type Game = (Time, Distance)
data Input = Input [Game] deriving (Eq, Ord, Show)

-- Functions

getResults :: Game -> Map Hold Distance
getResults (time, _) = let
  speeds = [0..time]
  timesRunning = map (time -) speeds
  distances = zipWith (*) speeds timesRunning
  in
    Map.fromList $ zip speeds distances

getWinningOptions :: Game -> Map Hold Bool
getWinningOptions game@(time, distance) = let
  results = getResults game
  in
    Map.mapWithKey (\_ d -> d > distance) results

getAllResults :: Input -> [Map Hold Distance]
getAllResults (Input games) = map getResults games

getAllWinningOptions :: Input -> [Map Hold Bool]
getAllWinningOptions (Input games) = map getWinningOptions games

computeOptions :: [Map Hold Bool] -> [Int]
computeOptions games = map f games where
  f game = let 
    wins = filter id $ Map.elems game
    in 
      length wins

solPart2 :: Input -> Int
solPart2 (Input games) = let 
  (times, distances) = unzip games
  time = read $ foldl (++) "" (map show times) :: Float
  distance = read $ foldl (++) "" (map show distances) :: Float
  root = sqrt (time * time - 4 * distance)
  t1 = ceiling $ (time - root) / 2
  t2 = ceiling $ (time + root) / 2
  in
    t2 - t1 

-- Parsing

type MParser = Parsec Void String

parseInput :: FilePath -> IO Input
parseInput inputFile = do
  fileContents <- lines <$> readFile inputFile
  let nonEmptyLines = filter (not . isEmpty) fileContents
  let trimmedLines = map trim nonEmptyLines
  let finalString = unlines trimmedLines
  case runParser mainParser inputFile finalString of
    Left s -> error (show s)
    Right input -> return input

mainParser :: MParser Input
mainParser = do
  string "Time:" >> many spaceChar
  times <- some integer
  many newline >> string "Distance:" >> many spaceChar
  distances <- some integer
  return (Input $ zip times distances)


daySix :: Bool -> IO ()
daySix test = do 
  let day = "day6"
  let prefix = "data/"
  let fileName = if test then prefix ++ day ++ "/test.txt" else prefix ++ day ++ "/input.txt"
  input <- parseInput fileName
  let games = getAllWinningOptions input
  print $ foldr (*) 1 $ computeOptions games
  print $ solPart2 input