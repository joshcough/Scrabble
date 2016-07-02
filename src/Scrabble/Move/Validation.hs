{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Scrabble.Move.Validation where

import Data.List             (intersperse, partition)
import Scrabble.Board.Board
import Scrabble.Dictionary
import Scrabble.Move.WordPut

import qualified Data.Set as Set

type Validator =
    [TilePut]     -- ^ all the letters put down this turn
  -> Board Square -- ^ old board
  -> Board Square -- ^ new board
  -> Dict         -- ^ the dictionary
  -> Either String ()

noValidation :: Validator
noValidation _ _ _ _ = Right ()

standardValidation :: Validator
standardValidation = validateMove

-- | Validate a move
--  1) At least one tile must be connected
--  2) All tiles must lie on one line
--  3) On the first turn, one tile must be located on square (7,7)
--  4) Linear word must be unbroken (including tiles previously on the board)
--  5) On every turn after the first, at least one crossword must be formed
--  6) All words formed must be inside the dictionary
-- TODO: return all the errors.
validateMove :: [TilePut]     -- ^ all the letters put down this turn
              -> Board Square -- ^ old board
              -> Board Square -- ^ new board
              -> Dict         -- ^ the dictionary
              -> Either String ()
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
    -- else if validateBoard b' then x
    else Right ()

  -- so we don't have to calculate it twice
  emptyB = isBoardEmpty b

  legals :: [SquareLegality]
  legals = f . (b !) <$> pointsPlayedInMove where
    f s = SquareLegality s
      (emptySquare s)
      (or $ taken <$> neighbors b' (squarePos s))
      (squarePos s == centerPosition)

  centerSquarePlayed = or $ centerPlay <$> legals

  allConnected :: Bool
  allConnected = and $ connected <$> legals

  pointsPlayedInMove = tilePutPoint <$> move

  takenSquares :: [Square]
  takenSquares = square <$> filter (not . available) legals

  words = toWord <$> (Set.toList $ wordsAtPoints pointsPlayedInMove b')

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


validateBoard :: Dict -> Board Square -> Either String ()
-- validateBoard dict board = error "todo"
validateBoard _ _ = error "todo"
