
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module DayNine where
import ParserUtils
import Utils

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

type Value = Int
type Sequence = [Value]
type Input = [Sequence]

-- Functions

diff :: Sequence -> Sequence
diff xs = zipWith (-) (tail xs) xs

allZeros :: Sequence -> Bool
allZeros xs = all (==0) xs

diffsUntilZeros :: Sequence -> [Sequence]
diffsUntilZeros xs = go xs [xs] where
  go xs seqs = let
    diffX = diff xs
    in case allZeros diffX of 
      True -> seqs ++ [diffX]
      _    -> go diffX (seqs ++ [diffX])

predictNext :: Sequence -> Value
predictNext xs = let
  diffs = diffsUntilZeros xs
  in
    sum $ map last diffs

predictPrev :: Sequence -> Value
predictPrev xs = let
  diffs = diffsUntilZeros (reverse xs)
  in
    sum $ map last diffs

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
  seqs <- many parseSequence
  return seqs

parseSequence :: MParser Sequence
parseSequence = do
    values <- some signedInteger
    _ <- many newline
    return values

dayNine :: Bool -> IO ()
dayNine test = do 
  let day = "day9"
  let prefix = "data/"
  let fileName = if test then prefix ++ day ++ "/test.txt" else prefix ++ day ++ "/input.txt"
  input <- parseInput fileName
  print $ sum $ map predictNext input
  print $ sum $ map predictPrev input
