{-# LANGUAGE FlexibleInstances #-}

module Scrabble.Types (
  module Scrabble.Position,
  Word,
  Points,
  Score,
  Dict,
  Letter,
  HasLetter(..),
  Orientation(..),
  catOrientation
) where

import Data.Maybe (Maybe)
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import Prelude hiding (Word)
import Scrabble.Position

type Letter = Char
type Word   = String
type Points = Int
type Score  = Int
type Dict   = Set Word
data Orientation = Horizontal | Vertical deriving (Eq, Show)

class HasLetter a where
  letter :: a -> Letter

catOrientation :: a -> a -> Orientation -> a
catOrientation l _ Horizontal = l
catOrientation _ r Vertical   = r
