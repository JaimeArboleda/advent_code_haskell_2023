
module DayFour where

import qualified Data.Text as T
import Data.List
import Data.Char
import qualified Data.Map as Map

data Card = Card Int [Int] [Int] deriving (Eq, Show, Ord)

readNumbers :: T.Text -> [Int]
readNumbers text = map (read . T.unpack) (T.words text)

parseCard :: T.Text -> Card
parseCard line = let
  (front : winning : have) = T.split (\c -> c==':' || c=='|') line
  cardId = (read . T.unpack . (T.drop 4) $ front) :: Int
  in 
    Card cardId (readNumbers winning) (readNumbers (head have))

parseInput :: String -> [Card]
parseInput = (map parseCard) . T.lines . T.pack

numWinning :: Card -> Int
numWinning (Card _ winning have) = length $ (filter id) $ map (\n -> elem n winning) have

getValueCard :: Card -> Int
getValueCard card = let
  won = numWinning card
  in 
    if won == 0 then 0 else 2^(won-1)

type Maze = Map.Map Card Int 
initialMaze :: [Card] -> Maze
initialMaze cards = Map.fromList $ zip cards (repeat 1)

buildMaze :: [Card] -> Maze
buildMaze cards = buildMazeRec cards 1 (initialMaze cards) where
  buildMazeRec cards pos maze = case pos == (length cards) of
    True -> maze
    False -> let 
      card = cards!!(pos-1)
      numW = numWinning card
      (Just copies) = Map.lookup card maze
      newMaze = Map.mapWithKey updatingFunc maze where
        updatingFunc (Card cardId _ _) v = if (cardId-pos) <= numW && (cardId-pos)>0 
          then (v+copies)
          else v
      in
        buildMazeRec cards (pos+1) newMaze

getValueMaze :: Maze -> Int
getValueMaze = sum . Map.elems

dayFour :: Bool -> IO ()
dayFour test = do 
  let fileName = if test then "data/day4/test.txt" else "data/day4/input.txt"
  dayInput <- readFile fileName
  let input = parseInput dayInput
  print (sum $ map getValueCard input)
  let maze = buildMaze input
  print (getValueMaze maze)