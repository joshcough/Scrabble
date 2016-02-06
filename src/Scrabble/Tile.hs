{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

-- | Tile and Bag representation
module Scrabble.Tile
  (
    module Scrabble.Dictionary
  , Points
  , Score
  , Tile(..)
  , HasLetter(..)
  , fromLetter
  , mkTile
  , points
  , simpleWordPoints
  , tileFromChar
  , tileFromCharEither
  , tilesFromJSON
  , tilesToJSON
  , tilesToString
  ) where

import Data.Aeson (ToJSON, FromJSON, Value(..), toJSON, parseJSON, withText)
import Data.Aeson.Types (Parser)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, listToMaybe)
import Data.Text (pack, unpack)
import GHC.Generics
import Scrabble.Dictionary
import Prelude hiding (Word)

import Debug.Trace

type Points = Int
type Score  = Int

data Tile = Tile { tileLetter :: Letter, score :: Int }
  deriving (Eq, Ord, Generic)

instance ToJSON Tile where
  toJSON (Tile l _) = String . pack $ show l

instance FromJSON Tile where
  parseJSON = withText "tile" $ \t ->
    let s   = unpack t in
      maybe (fail $ "invalid tile: " ++ s)
            return
            (listToMaybe s >>= tileFromChar)

instance HasLetter Tile where
  letter (Tile l _) = l

instance Show Tile where
  show (Tile l _) = show l

tilesToString :: [Tile] -> String
tilesToString tiles = (toChar . tileLetter) <$> tiles

tilesToJSON :: [Tile] -> Value
tilesToJSON = String . pack . tilesToString

tilesFromJSON ::
     String             -- ^ type
  -> (String -> String) -- ^ err message function
  -> ([Tile] -> a)      -- ^ constructor
  -> Value              -- ^ json value to parse
  -> Parser a
tilesFromJSON typ errMsg cons =
  withText typ $ \t -> let s = unpack t in
    maybe (fail $ errMsg s)
          (return . cons)
          (sequence $ tileFromChar <$> s)

mkTile :: Letter -> Tile
mkTile l = Tile l (fromJust $ Map.lookup l points)

fromLetter :: Letter -> Tile
fromLetter = mkTile

tileFromChar :: Char -> Maybe Tile
tileFromChar c = mkTile <$> fromChar c

tileFromCharEither :: Char -> Either String Tile
tileFromCharEither c =
  maybe (Left $ "invalid character: " ++ [c]) Right (tileFromChar c)

points :: Map Letter Points
points = Map.fromList [
  (A,1),(B,3), (C,3),(D,2),(E,1),
  (F,4),(G,2), (H,4),(I,1),(J,8),
  (K,5),(L,1), (M,3),(N,1),(O,1),
  (P,3),(Q,10),(R,1),(S,1),(T,1),
  (U,1),(V,4), (W,4),(X,8),(Y,4),
  (Z,10), (Blank, 0)]

simpleWordPoints :: Word -> Points
simpleWordPoints = sum . fmap f where
  f l = fromJust $ Map.lookup l points
