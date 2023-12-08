
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module DayEight where
import ParserUtils
import Utils

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Data.List
import Data.Void
import Data.Maybe
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad
import Control.Monad.Trans.State
import Debug.Trace


-- Data
type Position = String
data Move = MoveL | MoveR deriving (Eq, Ord, Show)
type Geography = Map Position (Position, Position) 
type Itinerary = [Move]
type Path = [Position]
data Input = Input Itinerary Geography deriving (Eq, Ord, Show)
type CurrentState = (Itinerary, Geography, Position, Path, Itinerary)
type StateUpdate = State CurrentState ()

-- Functions
getPos :: CurrentState -> Position
getPos (_, _, pos, _, _) = pos

getPath :: CurrentState -> Path
getPath (_, _, _, path, _) = path

getMoves :: CurrentState -> Itinerary
getMoves (_, _, _, _, moves) = moves

getNextPos :: Geography -> Position -> Move -> Position
getNextPos geo pos move = let
  Just (posL, posR) = Map.lookup pos geo
  in 
    case move of
      MoveL -> posL
      MoveR -> posR

next :: CurrentState -> CurrentState
next (itinerary, geo, pos, path, moves) = let
  move = head itinerary
  nextItinerary = tail itinerary
  nextPos = getNextPos geo pos move
  in
    (nextItinerary, geo, nextPos, path ++ [nextPos], moves ++ [move])

firstState :: Input -> CurrentState
firstState (Input itinerary geo) = (cycle itinerary, geo, "AAA", ["AAA"], [])

moveWhile :: (CurrentState -> Bool) -> State CurrentState ()
moveWhile cond = do
  s <- get
  when (cond s) (modify next >> moveWhile cond)

moveAll :: Input -> (Path, Itinerary)
moveAll input = let 
  first = firstState input
  (_, lastState) = runState (moveWhile (\s -> (getPos s) /= "ZZZ")) (firstState input)
  in
    (getPath lastState, getMoves lastState)

moveN :: Input -> Int -> (Path, Itinerary)
moveN input n = let 
  action = replicateM_ n (modify next) 
  (_, lastState) = runState action (firstState input)
  in 
    (getPath lastState, getMoves lastState)

-- Part 2 (without LCM trick, takes too long...)

type CurrentStatePt2 = (Itinerary, Geography, [Position], Itinerary)
type StateUpdatePt2 = State CurrentStatePt2 ()

getPosPt2 :: CurrentStatePt2 -> [Position]
getPosPt2 (_, _, poss, _) = poss

getMovesPt2 :: CurrentStatePt2 -> Itinerary
getMovesPt2 (_, _, _, moves) = moves

getNextPosPt2 :: Geography -> [Position] -> Move -> [Position]
getNextPosPt2 geo poss move = let
  possLR = catMaybes $ map (\p -> Map.lookup p geo) poss
  in 
    case move of
      MoveL -> map fst possLR
      MoveR -> map snd possLR

nextPt2 :: CurrentStatePt2 -> CurrentStatePt2
nextPt2 (itinerary, geo, poss, moves) = let
  move = head itinerary
  nextItinerary = tail itinerary
  nextPoss = getNextPosPt2 geo poss move
  in
    (nextItinerary, geo, nextPoss, moves ++ [move])

firstStatePt2 :: Input -> CurrentStatePt2
firstStatePt2 (Input itinerary geo) = (
  cycle itinerary, 
  geo, 
  filter (\k -> last k == 'A') (Map.keys geo), 
  [])

moveWhilePt2 :: (CurrentStatePt2 -> Bool) -> State CurrentStatePt2 ()
moveWhilePt2 cond = do
  s <- get
  when (cond s) (modify nextPt2 >> moveWhilePt2 cond)

stopCond :: CurrentStatePt2 -> Bool
stopCond (_, _, poss, _) = any (\p -> last p /= 'Z') poss

moveAllPt2 :: Input -> Itinerary
moveAllPt2 input = let 
  first = firstStatePt2 input
  (_, lastState) = runState (moveWhilePt2 stopCond) first
  in
    getMovesPt2 lastState

moveNPt2 :: Input -> Int -> ([Position], Itinerary)
moveNPt2 input n = let 
  action = replicateM_ n (modify nextPt2) 
  (_, lastState) = runState action (firstStatePt2 input)
  in 
    (getPosPt2 lastState, getMovesPt2 lastState)

-- Part 2 with LCM trick

computePt2 :: Input -> [Int]
computePt2 input@(Input itinerary geo) = let
  initState pos = (cycle itinerary, geo, pos, [pos], [])
  initialPoss = filter (\k -> last k == 'A') (Map.keys geo) 
  initialStates = map initState initialPoss
  cond s = last (getPos s) /= 'Z'
  computeCycle pos = (length . getMoves . snd) $ runState (moveWhile cond) (initState pos)
  in
    map computeCycle initialPoss

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
  moves <- many parseMove
  _ <- many newline
  nodes <- many parseGeo
  return $ Input moves (Map.fromList nodes)

parseMove :: MParser Move 
parseMove = do 
  r <- oneOf ("LR" :: String)
  return $ if (r=='L') then MoveL else MoveR

parseGeo :: MParser (Position, (Position, Position))
parseGeo = do
  pos <- many letterChar
  _ <- string " = ("
  posL <- many letterChar
  _ <- string ", "
  posR <- many letterChar
  _ <- (consumeUntil (\c -> c/='\n')) >> many newline
  return (pos, (posL, posR))


dayEight :: Bool -> IO ()
dayEight test = do 
  let day = "day8"
  let prefix = "data/"
  let fileName = if test then prefix ++ day ++ "/test.txt" else prefix ++ day ++ "/input.txt"
  input <- parseInput fileName
  let itinerary = snd $ moveAll input
  print $ length itinerary
  print $ foldr lcm 1 (computePt2 input)