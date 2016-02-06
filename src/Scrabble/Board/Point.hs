-------------------------------------------------------------------------------
-- |
-- Module      :  Scrabble.Board.Point
-- Copyright   :  (C) 2015-16 Joshua Cough
-- License     :  MIT License (see the file LICENSE)
-- Maintainer  :  Joshua Cough <joshcough@gmail.com>
--
-- Simple code to represent (x,y) coordinates on a plane
-- where the coordinate (0,0) is in the upper left hand corner
-- for example in this grid:
--
--   a,b,c
--   d,e,f
--   g,h,i
--
-- a is at position (0,0), and i is at position (2,2)
--
-- Therefore, as you go up vertically, y decreases.
-- There are no negative positions in such a plane.
-- This works well for a Scrabble board, but obviously
-- wouldn't work for all geometric needs.
--
-- However, all functions in this module will return negative values,
-- even though those are not valid for a scrabble board. This choice
-- was made out of convenience, instead of returning Maybe Point.
-- This would have been too annoying and pervasive.
-------------------------------------------------------------------------------
module Scrabble.Board.Point (
   Point
 -- * functions that return a single point
 , aboveP
 , belowP
 , leftOfP
 , rightOfP
 -- ** functions that return lists of points
 , allAboveP
 , allBelowP
 , allRightOfP
 , allLeftOfP
 , neighbors4P
) where

type Point = (Int, Int)

----------------------------------------
-- Functions that return a single point
----------------------------------------

-- | return the point directly above the given point
-- for example, in the example grid, b is directly above e.
aboveP :: Point -> Point
aboveP   (x, y) = (x, y - 1)
-- | return the point directly below the given point
-- for example, in the example grid, h is directly below e.
belowP :: Point -> Point
belowP   (x, y) = (x, y + 1)
-- | return the point directly above the given point
-- for example, in the example grid, b is directly above e.
leftOfP :: Point -> Point
leftOfP  (x, y) = (x - 1, y)
-- | return the point directly above the given point
-- for example, in the example grid, b is directly above e.
rightOfP :: Point -> Point
rightOfP (x, y) = (x + 1, y)

-----------------------------------------
-- Functions that return lists of points
-----------------------------------------

-- | infinite list of points above (x,y)
-- [ (x, y - 1), (x, y - 2) .. ]
-- example:
-- *ghci> take 10 $ allAboveP (7,7)
-- [(7,6),(7,5),(7,4),(7,3),(7,2),(7,1),(7,0),(7,-1),(7,-2),(7,-3)]
allAboveP :: Point -> [Point]
allAboveP (x, y) = zip (repeat x) [y - 1, y - 2 .. ]

-- | infinite list of points below (x,y)
-- [ (x, y + 1), (x, y + 2) .. ]
allBelowP :: Point -> [Point]
allBelowP (x, y) = zip (repeat x) [y + 1, y + 2 .. ]

-- | infinite list of points right of (x,y)
-- [ (x + 1, y), (x + 2, y) .. ]
allRightOfP :: Point -> [Point]
allRightOfP (x, y) = zip [x + 1, x + 2 .. ] (repeat y)

-- | infinite list of points left of (x,y)
-- [ (x - 1, y), (x - 2, y) .. ]
allLeftOfP :: Point -> [Point]
allLeftOfP (x, y) = zip [x - 1, x - 2 .. ] (repeat y)

-- | the four points directly above, below, left and right of the given point.
neighbors4P :: Point -> [Point]
neighbors4P p = [aboveP p, belowP p, leftOfP p, rightOfP p]