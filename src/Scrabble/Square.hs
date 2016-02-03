{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

-- | Square representation
module Scrabble.Square
  (
    Bonus(..)
  , Square(..)
  , debugSquare
  , emptySquare
  , showSquare
  , taken
  , toWord
  ) where

import Data.Aeson (ToJSON, FromJSON)
import qualified Data.Maybe as Maybe
import GHC.Generics
import Scrabble.Bag
import Scrabble.Position

data Bonus  = W3 | W2 | L3 | L2 | Star | NoBonus
  deriving (Eq, Ord, Generic, ToJSON, FromJSON)

instance Show Bonus where
  show W3      = "3W"
  show W2      = "2W"
  show L3      = "3L"
  show L2      = "2L"
  show Star    = " *"
  show NoBonus = "  "

data Square = Square {
  tile      :: Maybe Tile,
  bonus     :: Bonus,
  squarePos :: Point
} deriving (Eq, Ord, Generic, ToJSON, FromJSON)

instance Show Square where
  show = showSquare True

emptySquare :: Square -> Bool
emptySquare (Square Nothing _ _) = True
emptySquare _                    = False

taken :: Square -> Bool
taken = not . emptySquare

showSquare :: Bool -> Square -> String
showSquare printBonus (Square mt b _) =
  maybe (if printBonus then show b else "  ") (\t -> ' ' : show (letter t)) mt

debugSquare :: Square -> String
debugSquare (Square mt b p) = concat
  ["Square {tile: ", show mt, ", bonus: ", show b, ", pos: ", show p]

toWord :: [Square] -> [Letter]
toWord sqrs = letter <$> Maybe.catMaybes (tile <$> sqrs)
