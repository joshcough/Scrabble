{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Scrabble.Move.MoveHelpers where

import Data.Set (Set)
import qualified Data.Set as Set
import Scrabble.Board
import Scrabble.Matrix

wordsPlayedInTurn :: (Foldable b, Board b, Vec (Row b)) =>
  [Square] -> -- all the squares a player placed tiles in this turn
  b Square -> -- the board (with those tiles on it)
  Set [Square]
wordsPlayedInTurn squaresPlayedThisTurn nextBoard =
  Set.fromList . concat $ f <$> squaresPlayedThisTurn where
    f s = getWordsTouchingSquare s nextBoard

