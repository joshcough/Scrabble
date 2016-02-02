{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

-- | Tile and Bag representation
module Scrabble.Bag
  (
    Bag(..)
  , Points
  , Rack
  , Score
  , Tile(..)
  , HasLetter(..)
  , bagSize
  , countLettersInBag
  , fromLetter
  , newBag
  , newShuffledBag
  , orderedBag
  , orderedTiles
  , pointsInBag
  , simpleWordPoints
  , tileFromChar
  ) where

import Data.Aeson (ToJSON, FromJSON)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import GHC.Generics
import Scrabble.Dictionary
import System.Random.Shuffle
import Prelude hiding (Word)

type Rack   = [Tile]
type Points = Int
type Score  = Int

data Tile = Tile { tileLetter :: Letter, score :: Int }
  deriving (Eq, Ord, Generic, ToJSON, FromJSON)

data Bag = Bag { bagTiles :: [Tile] }
  deriving (Eq, Ord, Generic, ToJSON, FromJSON, Show)

instance HasLetter Tile where
  letter (Tile l _) = l

instance Show Tile where
  show (Tile l _) = show l

mkTile :: Letter -> Tile
mkTile l = Tile l (fromJust $ Map.lookup l points)

fromLetter :: Letter -> Tile
fromLetter = mkTile

tileFromChar :: Char -> Maybe Tile
tileFromChar c = mkTile <$> fromChar c

newBag :: IO Bag
newBag = newShuffledBag

newShuffledBag :: IO Bag
newShuffledBag = Bag <$> shuffleM orderedTiles

orderedBag :: Bag
orderedBag = Bag $ orderedTiles

orderedTiles :: [Tile]
orderedTiles = concat $ f <$> distribution where
  f (l,n) = fmap mkTile $ replicate n l

distribution :: [(Letter,Int)]
distribution = [
  (A,9),(B,2),(C,2),(D,4),(E,12),
  (F,2),(G,3),(H,2),(I,9),(J,1),
  (K,1),(L,4),(M,2),(N,6),(O,8),
  (P,2),(Q,1),(R,6),(S,4),(T,6),
  (U,4),(V,2),(W,2),(X,1),(Y,2),(Z,1), (Blank, 2)]

countLettersInBag :: Letter -> Bag -> Int
countLettersInBag l (Bag b) = length (filter (==l) $ map letter b)

points :: Map Letter Points
points = Map.fromList [
  (A,1),(B,3), (C,3),(D,2),(E,1),
  (F,4),(G,2), (H,4),(I,1),(J,8),
  (K,5),(L,1), (M,3),(N,1),(O,1),
  (P,3),(Q,10),(R,1),(S,1),(T,1),
  (U,1),(V,4), (W,4),(X,8),(Y,4),
  (Z,10), (Blank, 0)]

pointsInBag :: Bag -> Int
pointsInBag (Bag b) = sum $ fmap score b

simpleWordPoints :: Word -> Points
simpleWordPoints = sum . fmap f where
  f l = fromJust $ Map.lookup l points

bagSize :: Bag -> Int
bagSize (Bag b) = length b