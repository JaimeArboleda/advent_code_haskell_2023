{-# LANGUAGE OverloadedStrings #-}
module DayFive where
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

type RawId = Int
type Seeds = [RawId]
type Category = String
type Span = Int
data Mapping = Mapping RawId RawId Span deriving (Eq, Ord, Show)
type Mappings = [Mapping]
data Input = Input Seeds (Map (Category, Category) Mappings) deriving (Eq, Ord, Show)

categories :: [Category]
categories = ["seed", "soil", "fertilizer", "water", "light", "temperature", "humidity", "location"]

dependencies :: Map Category Category
dependencies = Map.fromList $ zip categories (drop 1 categories)

lookupMappings :: Mappings -> RawId -> RawId
lookupMappings [] id_source = id_source
lookupMappings (x:xs) id_source = let
  Mapping first_destination first_source range = x 
  offset = id_source - first_source
  in 
    if offset >= 0 && offset < range
      then first_destination + offset
      else lookupMappings xs id_source

convertId :: Input -> Category -> RawId -> RawId
convertId (Input _ allMappings) category id_source = let 
  Just dependency = Map.lookup category dependencies
  Just mappings = Map.lookup (category, dependency) allMappings 
  in 
    lookupMappings mappings id_source

dependencyGraph :: Input -> RawId -> Map Category RawId
dependencyGraph input seed = Map.fromList (f input seed categories) where
  f input rawId [] = []
  f input rawId (cat:cats) = let
    newCat = Map.lookup cat dependencies
    in
      case newCat of
        Just c -> let
          newId = convertId input cat rawId
          in 
            (cat, rawId):(f input newId cats)
        Nothing -> [(cat, rawId)]

getLocation :: Input -> RawId -> RawId
getLocation input seed = let
  graph = dependencyGraph input seed
  Just location = Map.lookup "location" graph
  in
    location

allLocations :: Input -> [RawId]
allLocations input@(Input seeds _) = map (getLocation input) seeds

-- Part 2
type RangeSpan = (RawId, Span)
type RangePair = (RawId, RawId)
type Offset = Int

convertToSpan :: RangePair -> RangeSpan
convertToSpan (a, b) = (a, b - a + 1)

convertToPair :: RangeSpan -> RangePair
convertToPair (a, span) = (a, a + span - 1)

getOffsetM :: Mapping -> Offset
getOffsetM (Mapping x y _) = x - y 

addOffset :: Offset -> RangeSpan -> RangeSpan
addOffset offset (a, span) = (a + offset, span)

overlap :: Mapping -> RangeSpan -> Maybe (RangeSpan, Offset)
overlap mapping@(Mapping _ a0 span0) (a1, span1) = let
  (_, b0) = convertToPair (a0, span0)
  (_, b1) = convertToPair (a1, span1)
  a0b1 = compare a0 b1
  b0a1 = compare b0 a1
  in 
    case (a0b1, b0a1) of
      (GT, _) -> Nothing 
      (_, LT) -> Nothing 
      otherwise -> Just (convertToSpan (max a0 a1, min b0 b1), getOffsetM mapping)

convertRange :: Input -> Category -> RangeSpan -> [RangeSpan]
convertRange input@(Input _ allMappings) category range_source@(rawId, span) = let 
  Just dependency =  Map.lookup category dependencies
  Just mappings =   Map.lookup (category, dependency) allMappings 
  overlaps = catMaybes $ map (\m -> overlap m range_source) mappings
  in 
    if length overlaps == 0 
      then [range_source]
      else let
        (ovRange@(ovId, ovSpan), offset) = minimumBy (compare `on` (fst . fst)) overlaps
        add0 = if (rawId < ovId) then [(rawId, ovId - rawId)] else []
        needsRecCall = (ovId + ovSpan) < (rawId + span)
        add1 = [addOffset offset ovRange]
        addPrev = add0 ++ add1
        remainder = convertToSpan (ovId + ovSpan, rawId + span - 1)
        in 
          if needsRecCall 
            then addPrev ++ (convertRange input category remainder)
            else addPrev

computeNextRanges :: Input -> Category -> [RangeSpan] -> [RangeSpan]
computeNextRanges input cat [] = []
computeNextRanges input cat (range:ranges) = (convertRange input cat range) 
                                             ++ (computeNextRanges input cat ranges)

computeSeedRanges :: Input -> [RangeSpan]
computeSeedRanges (Input seeds _) = f seeds where
  f [] = []
  f (a:span:rest) = [(a, span)] ++ (f rest)

computeLocationRanges :: Input -> [RangeSpan]
computeLocationRanges input = let
  seedRanges = computeSeedRanges input
  in 
    iterateCats seedRanges categories where
      iterateCats ranges [] = []
      iterateCats ranges (cat:cats) = let
        newCat =  Map.lookup cat dependencies
        in
          case newCat of
            Just c -> iterateCats (computeNextRanges input cat ranges) cats
            Nothing -> ranges

minLocation :: Input -> RawId
minLocation input = let
  locationRanges = computeLocationRanges input
  in
    minimum $ map fst locationRanges

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
  string "seeds: "
  seeds <- some integer
  _ <- many newline
  allMappings <- many mappingParser
  return (Input seeds (Map.fromList allMappings))

mappingParser :: MParser ((Category, Category), Mappings)
mappingParser = do
  source <- manyTill letterChar (char '-')
  _ <- manyTill letterChar (char '-')
  destination <- manyTill letterChar (char ' ')
  _ <- consumeLine
  mappings <- many mappingLineParser
  return ((source, destination), mappings)

mappingLineParser :: MParser Mapping
mappingLineParser = do
  destination_id <- integer
  source_id <- integer
  range <- integer
  _ <- newline
  return $ Mapping destination_id source_id range



dayFive :: Bool -> IO ()
dayFive test = do 
  let fileName = if test then "data/day5/test.txt" else "data/day5/input.txt"
  input <- parseInput fileName
  let locations = allLocations input
  print (minimum locations)
  print (minLocation input)