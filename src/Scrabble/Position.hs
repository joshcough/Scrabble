-- | Simple code to represent (x,y) coordinates.
module Scrabble.Position (
  x,y,aboveP,belowP,leftOfP,rightOfP,neighborsP,Point
) where

type Point = (Int, Int)

x :: (a, b) -> a
x (a, _) = a
y :: (a, b) -> b
y (_, a) = a
aboveP :: Point -> Point
aboveP   (x, y) = (x, y - 1)
belowP :: Point -> Point
belowP   (x, y) = (x, y + 1)
leftOfP :: Point -> Point
leftOfP  (x, y) = (x - 1, y)
rightOfP :: Point -> Point
rightOfP (x, y) = (x + 1, y)

neighborsP :: Point -> [Point]
neighborsP p = [aboveP p, belowP p, leftOfP p, rightOfP p]


