module Scrabble.Bag where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Scrabble.Types
import System.Random.Shuffle

type Bag = [Letter]

newBag :: IO Bag
newBag = newShuffledBag

isBagEmpty :: Bag -> Bool
isBagEmpty = null

newShuffledBag :: IO Bag
newShuffledBag = shuffleM orderedBag

distribution :: [(Letter,Int)]
distribution = [
  ('A',9),('B',2),('C',2),('D',4),('E',12),
  ('F',2),('G',3),('H',2),('I',9),('J',1),
  ('K',1),('L',4),('M',2),('N',6),('O',8),
  ('P',2),('Q',1),('R',6),('S',4),('T',6),
  ('U',4),('V',2),('W',2),('X',1),('Y',2),('Z',1), (' ', 2)]

distributionMap :: Map Letter Int
distributionMap = Map.fromList distribution

orderedBag :: Bag
orderedBag = concat $ fmap (\(l,n) -> replicate n l) distribution where

points :: [(Letter,Points)]
points = [
  ('A',1),('B',3), ('C',3),('D',2),('E',1),
  ('F',4),('G',2), ('H',4),('I',1),('J',8),
  ('K',5),('L',1), ('M',3),('N',1),('O',1),
  ('P',3),('Q',10),('R',1),('S',1),('T',1),
  ('U',1),('V',4), ('W',4),('X',8),('Y',4),('Z',10), (' ', 0)]

pointsMap :: Map Letter Points
pointsMap = Map.fromList points

distributionWithPoints :: Map Letter (Int,Points)
distributionWithPoints = Map.intersectionWith (,) distributionMap pointsMap

totalPoints :: Int
totalPoints = sum $ Map.map (uncurry (*)) distributionWithPoints
