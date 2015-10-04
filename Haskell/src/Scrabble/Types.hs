{-# LANGUAGE FlexibleInstances #-}

module Scrabble.Types (
  module Scrabble.Position,
  Tray,
  Word,
  Points,
  Score,
  Dict,
  Letter,
  HasLetter(..),
  Orientation(..),
  catOrientation,
  Tile(..),
  PutTile(..),
  PutWord(..)
) where

import Scrabble.Position
import Prelude hiding (Word)

type Letter = Char
type Tray   = [Tile]
type Word   = String
type Points = Int
type Score  = Int
type Dict   = [Word]
data Orientation = Horizontal | Vertical deriving (Eq, Show)

class HasLetter a where
  letter :: a -> Letter

data Tile = Tile { _tileLetter :: Letter, score :: Int } deriving (Eq,Ord)

instance HasLetter Tile where
  letter (Tile l _) = l

instance Show Tile where
  show (Tile letter _) = [letter]

catOrientation :: a -> a -> Orientation -> a
catOrientation l _ Horizontal = l
catOrientation _ r Vertical   = r

{-
 represents a single tile being put on the board (without location)
   * PutLetterTile means the tile has a letter on it
   * PutBlankTile comes with the Letter that the player intends use.
-}
data PutTile =
   PutLetterTile Tile   Position
 | PutBlankTile  Letter Position
  deriving Eq

-- TODO: when are these shown? show the position be shown too?
instance Show PutTile where
  show (PutLetterTile t _) = [letter t]
  show (PutBlankTile  l _) = [l]

instance HasLetter PutTile where
  letter (PutLetterTile t _) = letter t
  letter (PutBlankTile  l _) = l

instance HasPosition PutTile where
  pos (PutLetterTile _ p) = p
  pos (PutBlankTile  _ p) = p

{- A complete representation of placing a word on the board. -}
data PutWord = PutWord { tiles :: [PutTile] } deriving Show

