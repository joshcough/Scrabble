module Scrabble.Types where

import Prelude hiding (Word)

data Tile = Tile { letter :: Letter, score :: Int } deriving Eq

instance Show Tile where
  show (Tile letters _) = show letters

data Position = Position { x :: Int, y :: Int } deriving (Eq, Show)

type Letter = Char
type Tray   = [Tile]
type Word   = String
type Points = Int
type Score  = Int
type Dict   = [Word]

data Orientation = Horizontal | Vertical deriving Show

data Pattern  = Pattern { tiles :: [Maybe Tile] }

instance Show Pattern where
  show (Pattern ts) = fmap f ts where
    f Nothing  = '@'
    f (Just (Tile l _)) = l

