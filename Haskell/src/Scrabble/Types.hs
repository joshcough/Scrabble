{-# LANGUAGE FlexibleInstances #-}

module Scrabble.Types where

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

class HasPosition a where
  pos :: a -> Position

data Tile = Tile { _tileLetter :: Letter, score :: Int } deriving (Eq,Ord)

instance HasLetter Tile where
  letter (Tile l _) = l

instance Show Tile where
  show (Tile letter _) = [letter]

data Position = Position { posX :: Int, posY :: Int } deriving (Eq, Ord, Show)

instance HasPosition Position where
  pos = id

instance HasPosition (Int, Int) where
  pos (x,y) = Position x y

class Pos a where
  coors    :: a -> (Int, Int)
  x        :: a -> Int
  y        :: a -> Int
  aboveP   :: a -> a
  belowP   :: a -> a
  leftOfP  :: a -> a
  rightOfP :: a -> a

instance Pos (Int, Int) where
  coors    (x, y) = (x, y)
  x        (a, _) = a
  y        (_, a) = a
  aboveP   (x, y) = (x, y - 1)
  belowP   (x, y) = (x, y + 1)
  leftOfP  (x, y) = (x - 1, y)
  rightOfP (x, y) = (x + 1, y)

instance Pos Position where
  coors    (Position x y) = (x, y)
  x        (Position a _) = a
  y        (Position _ a) = a
  aboveP   (Position x y) = Position x (y - 1)
  belowP   (Position x y) = Position x (y + 1)
  leftOfP  (Position x y) = Position (x - 1) y
  rightOfP (Position x y) = Position (x + 1) y

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

