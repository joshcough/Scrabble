{-# LANGUAGE FlexibleInstances #-}

module Scrabble.Types where

import Data.Ix
import Prelude hiding (Word)

class HasLetter a where
  letter :: a -> Letter

data Tile = Tile { _tileLetter :: Letter, score :: Int } deriving (Eq,Ord)

instance HasLetter Tile where
  letter (Tile l _) = l

instance Show Tile where
  show (Tile letter _) = [letter]

data Position = Position { posX :: Int, posY :: Int } deriving (Eq, Show)

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

asTuple :: Position -> (Int, Int)
asTuple (Position x y) = (x,y)

fromTuple :: (Int, Int) -> Position
fromTuple (x,y) = Position x y

instance Ord Position where
  compare p1 p2 = compare (asTuple p1) (asTuple p2)

instance Ix Position where
  range   (p1, p2)   = fmap fromTuple $ range (asTuple p1, asTuple p2)
  index   (p1, p2) p = index   (asTuple p1, asTuple p2) (asTuple p)
  inRange (p1, p2) p = inRange (asTuple p1, asTuple p2) (asTuple p)

type Letter = Char
type Tray   = [Tile]
type Word   = String
type Points = Int
type Score  = Int
type Dict   = [Word]

data Orientation = Horizontal | Vertical deriving (Eq, Show)

catOrientation :: a -> a -> Orientation -> a
catOrientation l _ Horizontal = l
catOrientation _ r Vertical   = r

{-
 represents a single tile being put on the board (without location)
   * PutLetterTile means the tile has a letter on it
   * PutBlankTile comes with the Letter that the player intends use.
-}
data PutTile = PutLetterTile Tile | PutBlankTile Letter deriving Eq
instance Show PutTile where
  show (PutLetterTile t) = [letter t]
  show (PutBlankTile  l) = [l]

instance HasLetter PutTile where
  letter (PutLetterTile t) = letter t
  letter (PutBlankTile  l) = l

{-
 represents the tiles laid down when placing a tile
   * (Just t) means some tile was placed
   * Nothing means that the tile alreaady on the board is to be used
-}
data PutTiles = PutTiles { tiles :: [Maybe PutTile] }

instance Show PutTiles where
  show (PutTiles ts) = fmap f ts where
    f Nothing   = '@'            -- use whatever letter is '@' that position
    f (Just tp) = head $ show tp -- show the tile being laid down

{- A complete representation of placing a word on the board. -}
data PutWord = PutWord {
  _putWordTiles       :: PutTiles
 ,_putWordOrientation :: Orientation
 ,_putWordPosition    :: Position
} deriving Show

