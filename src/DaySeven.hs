
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module DaySeven where
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

data Card = Jack2 | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten
          | Jack | Queen | King | Ace deriving (Eq, Ord, Enum, Show)  

data Hand = Hand Card Card Card Card Card deriving (Eq, Show)

type Bid = Int

data Game = Game Hand Bid deriving (Eq, Show)

data Type = HighCard | Pair | TwoPair | Trio | Full | Poker | RePoker deriving (Eq, Ord, Enum, Show)
    
type Input = [Game]

-- Functions

getHand :: Game -> Hand
getHand (Game h _) = h

getBid :: Game -> Bid
getBid (Game _ b) = b

getCards :: Hand -> [Card]
getCards (Hand c0 c1 c2 c3 c4) = [c0, c1, c2, c3, c4]

fromCards :: [Card] -> Hand
fromCards (c0:c1:c2:c3:c4:[]) = Hand c0 c1 c2 c3 c4

getTypeSig :: Hand -> (Type, [Card])
getTypeSig hand = let
  cards = getCards hand
  cardCounts = countElements cards
  counts = Map.elems cardCounts
  five = 5 `elem` counts
  four = 4 `elem` counts
  three = 3 `elem` counts
  two = 2 `elem` counts
  twoP = (length $ filter (==2) counts) == 2
  signatureCards = reverse $ sortKeysByValue cardCounts
  in 
    (,signatureCards) $ case (five, four, three, two, twoP) of
      (True, _, _, _, _)    -> RePoker
      (_, True, _, _, _)    -> Poker
      (_, _, True, True, _) -> Full
      (_, _, True, _, _)    -> Trio
      (_, _, _, True, True) -> TwoPair
      (_, _, _, True, _)    -> Pair
      _                     -> HighCard
  
getTypeSig2 :: Hand -> (Type, [Card])
getTypeSig2 hand = let
  cards = getCards hand
  cardCounts = countElements cards
  (typeC, sigC) = getTypeSig hand
  numJacks = Map.findWithDefault 0 Jack cardCounts 
  newCards = if (sigC!!0 == Jack) && (length sigC > 1) 
    then replace Jack (sigC!!1) cards
    else replace Jack (sigC!!0) cards
  in 
    (fst . getTypeSig $ fromCards newCards,replace Jack Jack2 cards) 

compareHandReal :: Hand -> Hand -> Ordering
compareHandReal handX handY = let
-- Not used: but it's the real way of comparing hands
  (typeX, signatureX) = getTypeSig handX
  (typeY, signatureY) = getTypeSig handY
  in
    case compare typeX typeY of
      EQ -> lexComp signatureX signatureY
      _  -> compare typeX typeY

compareHand1 :: Hand -> Hand -> Ordering
compareHand1 handX handY = let
  (typeX, _) = getTypeSig handX
  (typeY, _) = getTypeSig handY
  cardsX = getCards handX
  cardsY = getCards handY
  in
    case compare typeX typeY of
      EQ -> lexComp cardsX cardsY
      _  -> compare typeX typeY

compareHand2 :: Hand -> Hand -> Ordering
compareHand2 handX handY = let
  (typeX, signatureX) = getTypeSig2 handX
  (typeY, signatureY) = getTypeSig2 handY
  in
      case compare typeX typeY of
      EQ -> lexComp signatureX signatureY
      _  -> compare typeX typeY

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
  games <- many parseGame
  return (games)

parseGame :: MParser Game
parseGame = do
    cards <- many parseCard
    _ <- spaceChar
    bid <- integer
    _ <- many newline
    return (Game (Hand (cards!!0) (cards!!1) (cards!!2) (cards!!3) (cards!!4)) bid)

parseCard :: MParser Card
parseCard = do 
  r <- oneOf ("23456789TJQKA" :: String)
  return $ case r of
    'T' -> Ten
    'J' -> Jack
    'Q' -> Queen
    'K' -> King
    'A' -> Ace
    n  -> toEnum (read [n] - 1)

daySeven :: Bool -> IO ()
daySeven test = do 
  let day = "day7"
  let prefix = "data/"
  let fileName = if test then prefix ++ day ++ "/test.txt" else prefix ++ day ++ "/input.txt"
  games <- parseInput fileName
  let sortedGames = sortBy (\x y -> compareHand1 (getHand x) (getHand y)) games
  print $ sum $ zipWith (*) (map getBid sortedGames) [1..(length games)]
  let sortedGames2 = sortBy (\x y -> compareHand2 (getHand x) (getHand y)) games
  print $ sum $ zipWith (*) (map getBid sortedGames2) [1..(length games)]