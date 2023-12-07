module Utils where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.List

countElements :: (Ord a) => [a] -> Map a Int
countElements = foldl' (\acc x -> Map.insertWith (+) x 1 acc) Map.empty

invertMap :: (Ord v) => Map k v -> Map v k
invertMap = Map.fromList . (map (uncurry $ flip (,))) . Map.assocs 

sortKeysByValue :: (Ord v, Ord k) => Map k v -> [k]
sortKeysByValue m = map fst $ sortBy comp $ Map.assocs m where
  comp (k, v) (k', v') = case compare v v' of
    EQ -> compare k k'
    _  -> compare v v'

lexComp :: (Ord a) => [a] -> [a] -> Ordering
lexComp [] [] = EQ
lexComp [] (y:ys) = LT
lexComp (x:xs) [] = GT
lexComp (x:xs) (y:ys) = case compare x y of
  EQ -> lexComp xs ys
  _  -> compare x y

replace :: (Eq a) => a -> a -> [a] -> [a]
replace _ _ [] = []
replace a b (x:xs) = if x==a then b:(replace a b xs) else x:(replace a b xs)