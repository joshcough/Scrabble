module Scrabble.Bag where

import qualified Data.Char as Char
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Scrabble.Types
import System.Random.Shuffle
import Prelude hiding (Word)

type Rack = [Tile]

data Tile = Tile { _tileLetter :: Letter, score :: Int } deriving (Eq,Ord)

instance HasLetter Tile where
  letter (Tile l _) = l

instance Show Tile where
  show (Tile letter _) = [letter]

mkTile :: Letter -> Tile
mkTile l = Tile l (fromMaybe err $ Map.lookup (Char.toUpper l) points) where
  err = error $ l : " is an invalid letter"

fromLetter :: Letter -> Tile
fromLetter = mkTile

type Bag = [Tile]

newBag :: IO Bag
newBag = newShuffledBag

emptyBag :: Bag
emptyBag = []

newShuffledBag :: IO Bag
newShuffledBag = shuffleM orderedBag

distribution :: [(Letter,Int)]
distribution = [
  ('A',9),('B',2),('C',2),('D',4),('E',12),
  ('F',2),('G',3),('H',2),('I',9),('J',1),
  ('K',1),('L',4),('M',2),('N',6),('O',8),
  ('P',2),('Q',1),('R',6),('S',4),('T',6),
  ('U',4),('V',2),('W',2),('X',1),('Y',2),('Z',1), ('_', 2)]

distributionMap :: Map Letter Int
distributionMap = Map.fromList distribution

orderedBag :: Bag
orderedBag = concat $ f <$> distribution where
  f (l,n) = fmap mkTile $ replicate n l

countLettersInBag :: Letter -> Bag -> Int
countLettersInBag l b = length (filter (==l) $ map letter b)

points :: Map Letter Points
points = Map.fromList [
  ('A',1),('B',3), ('C',3),('D',2),('E',1),
  ('F',4),('G',2), ('H',4),('I',1),('J',8),
  ('K',5),('L',1), ('M',3),('N',1),('O',1),
  ('P',3),('Q',10),('R',1),('S',1),('T',1),
  ('U',1),('V',4), ('W',4),('X',8),('Y',4),
  ('Z',10), ('_', 0)]

distributionWithPoints :: Map Letter (Int,Points)
distributionWithPoints =
  Map.intersectionWith (,) distributionMap points

totalPoints :: Int
totalPoints = sum $ Map.map (uncurry (*)) distributionWithPoints

simpleWordPoints :: Word -> Points
simpleWordPoints = sum . fmap f where
  f l = fromMaybe err $ Map.lookup (Char.toUpper l) points
  err = error "simple word points"
