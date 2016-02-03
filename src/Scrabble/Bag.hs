{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

-- | Tile and Bag representation
module Scrabble.Bag
  (
    module Scrabble.Tile
  , Bag(..)
  , Points
  , Rack(..)
  , Score
  , Tile(..)
  , HasLetter(..)
  , bagSize
  , countLettersInBag
  , fromLetter
  , newBag
  , newRack
  , newShuffledBag
  , orderedBag
  , orderedTiles
  , pointsInBag
  , simpleWordPoints
  , tileFromChar
  , tilesFromJSON
  , tilesToJSON
  ) where

import Data.Aeson (ToJSON, FromJSON, Value(..), toJSON, parseJSON, withText)
import Data.Aeson.Types (Parser)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, listToMaybe)
import Data.Text (pack, unpack)
import GHC.Generics
import Scrabble.Dictionary
import Scrabble.Tile
import System.Random.Shuffle
import Prelude hiding (Word)

data Rack = Rack { rackTiles :: [Tile] }
  deriving (Eq, Ord, Generic)

data Bag = Bag { bagTiles :: [Tile] }
  deriving (Eq, Ord, Generic, Show)

instance Show Rack where
  show (Rack tiles) = tilesToString tiles

instance ToJSON Rack where
  toJSON (Rack tiles) = tilesToJSON tiles

instance FromJSON Rack where
  parseJSON = tilesFromJSON "Rack" ("invalid tile in rack: " ++) Rack

instance ToJSON Bag where
  toJSON (Bag tiles) = tilesToJSON tiles

instance FromJSON Bag where
  parseJSON = tilesFromJSON "Bag" ("invalid tile in bag: " ++) Bag

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

pointsInBag :: Bag -> Int
pointsInBag (Bag b) = sum $ fmap score b

bagSize :: Bag -> Int
bagSize (Bag b) = length b

newRack :: Rack
newRack = Rack []