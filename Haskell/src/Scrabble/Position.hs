{-# LANGUAGE FlexibleInstances #-}

module Scrabble.Position where

class HasPosition a where
  pos :: a -> Position

data Position = Position {
  posX :: Int,
  posY :: Int
} deriving (Eq, Ord, Show)

instance HasPosition Position where
  pos = id

instance HasPosition (Int, Int) where
  pos (x,y) = Position x y

class Pos a where
  coors      :: a -> (Int, Int)
  x          :: a -> Int
  y          :: a -> Int
  aboveP     :: a -> a
  belowP     :: a -> a
  leftOfP    :: a -> a
  rightOfP   :: a -> a

instance Pos (Int, Int) where
  coors      (x, y) = (x, y)
  x          (a, _) = a
  y          (_, a) = a
  aboveP     (x, y) = (x, y - 1)
  belowP     (x, y) = (x, y + 1)
  leftOfP    (x, y) = (x - 1, y)
  rightOfP   (x, y) = (x + 1, y)

neighborsP :: Pos p => p -> [p]
neighborsP p = [aboveP p, belowP p, leftOfP p, rightOfP p]

instance Pos Position where
  coors      (Position x y) = (x, y)
  x          (Position a _) = a
  y          (Position _ a) = a
  aboveP     (Position x y) = Position x (y - 1)
  belowP     (Position x y) = Position x (y + 1)
  leftOfP    (Position x y) = Position (x - 1) y
  rightOfP   (Position x y) = Position (x + 1) y
