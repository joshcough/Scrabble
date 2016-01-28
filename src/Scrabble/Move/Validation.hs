{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Scrabble.Move.Validation where

import Data.List (intersperse, partition)
import qualified Data.Set as Set
import Prelude hiding (Word)
import Scrabble.Board
import Scrabble.Dictionary
import Scrabble.Matrix
import Scrabble.Move.MoveHelpers
import Scrabble.Move.WordPut
import Scrabble.Position

-- | Validate a move
--  1) At least one tile must be connected
--  2) All tiles must lie on one line
--  3) On the first turn, one tile must be located on square (7,7)
--  4) Linear word must be unbroken (including tiles previously on the board)
--  5) On every turn after the first, at least one crossword must be formed
--  6) All words formed must be inside the dictionary
-- TODO: return all the errors.
validateMove ::
  (Vec (Row b), Foldable b, Board b, Pos p, Show p) =>
  [(Square,TilePut,p)] -> -- ^ all the letters put down this turn
  b Square ->             -- ^ old board
  b Square ->             -- ^ new board
  Dict     ->             -- ^ the dictionary
  Either String ()
validateMove move b b' dict = go where
  go =
    if null move
      then Left "You must place at least one tile!"
    else if emptyB && not centerSquarePlayed
      then Left "Must use center square!"
    else if emptyB && length move <= 1
      then Left "First move must have more than one letter!"
    else if not allConnected
      then Left "Unconnected letters!"
    else if not $ null takenSquares
      then Left squaresTakenError
    else if not $ null badWords
      then Left $ "Bad words: " ++ show badWords
    else Right ()

  -- so we don't have to calculate it twice
  emptyB = emptyBoard b

  legals :: [SquareLegality]
  legals = f <$> squaresPlayedInMove where
    f s = SquareLegality s
      (emptySquare s)
      (or $ taken <$> neighbors b' (pos s))
      (pos s == centerPosition)

  centerSquarePlayed = or $ centerPlay <$> legals

  allConnected :: Bool
  allConnected = and $ connected <$> legals

  squaresPlayedInMove = (\(s,_,_) -> s) <$> move

  takenSquares :: [Square]
  takenSquares = square <$> filter (not . available) legals

  words :: [Word]
  words = toWord <$> (Set.toList $ wordsPlayedInTurn squaresPlayedInMove b')

  (_, badWords) = partition (dictContainsWord dict) words

  squaresTakenError = "Error, squares taken: " ++
    show (intersperse "," $ debugSquare <$> takenSquares)

-- | helper data for packing information about a
--   square played on a particular turn.
data SquareLegality = SquareLegality {
  square     :: Square
 ,available  :: Bool -- is the square not taken
 ,connected  :: Bool -- if touching another tile
 ,centerPlay :: Bool -- if square is the center square
} deriving Show
