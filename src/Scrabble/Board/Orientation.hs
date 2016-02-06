{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

-- | Board representation
module Scrabble.Board.Orientation
  (
    Orientation(..)
  , afterByOrientationP
  , allAfterByOrientationP
  , allBeforeByOrientationP
  , beforeByOrientationP
  , foldOrientation
  ) where

import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics
import Scrabble.Board.Point

data Orientation = Horizontal | Vertical
  deriving (Eq, Ord, Generic, ToJSON, FromJSON, Show)

foldOrientation :: a -> a -> Orientation -> a
foldOrientation l _ Horizontal = l
foldOrientation _ r Vertical   = r

-- | get the point before the given point, vertically or horizontally
beforeByOrientationP :: Orientation -> Point -> Point
beforeByOrientationP = foldOrientation leftOfP aboveP
-- | get the point after the given point, vertically or horizontally
afterByOrientationP :: Orientation -> Point -> Point
afterByOrientationP = foldOrientation rightOfP belowP

-- | get all the points before the given point, vertically or horizontally
allBeforeByOrientationP :: Orientation -> Point -> [Point]
allBeforeByOrientationP = foldOrientation allLeftOfP allAboveP
-- | get all the points after the given point, vertically or horizontally
allAfterByOrientationP :: Orientation -> Point -> [Point]
allAfterByOrientationP = foldOrientation allRightOfP allBelowP
