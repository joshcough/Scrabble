{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Scrabble.Move.Move (
  module Scrabble.Move.WordPut
 ,module Scrabble.Move.Validation
 ,Move(..)
 ,createMove
 ,wordPut
) where

import Data.List (delete, foldl')
import Prelude hiding (Word)
import Scrabble.Bag
import Scrabble.Board
import Scrabble.Dictionary
import Scrabble.Move.Scoring
import Scrabble.Move.Validation
import Scrabble.Move.WordPut
import Scrabble.Position
import Scrabble.Search (containsAll)

data Move = Move {
  moveWordPut         :: WordPut
 ,movePointsScored    :: Points
 ,moveRackAfter       :: Rack -- not yet refilled
 ,moveBoardAfterMove  :: Board }

-- | Attempt to lay tiles on the board.
--   Validate the entire move.
--   Calculate the score of the move.
wordPut :: Validator
       ->  Board    -- ^ the board to put the word on
       ->  WordPut  -- ^ the word being put on the board
       ->  Dict     -- ^ the scrabble dictionary
       ->  Either String (Board, Score)
wordPut validator b wp dict = do
  squares <- squaresPlayedThisTurn
  let b' = nextBoard $ zip squares (tiles wp)
  validator (zipSquaresAndTiles squares) b b' dict
  return (b', calculateTurnScore squares b') where

  squaresPlayedThisTurn :: Either String [Square]
  squaresPlayedThisTurn = traverse f (tilePutPoint <$> tiles wp) where
    f :: Point -> Either String Square
    f p = maybe (Left $ "out of bounds: " ++ show p) Right $ elemAt b p

  zipSquaresAndTiles :: [Square] -> [(Square,TilePut,Point)]
  zipSquaresAndTiles sqrs = zipWith f sqrs (tiles wp) where
    f s t = (s, t, tilePutPoint t)

  nextBoard :: [(Square,TilePut)] -> Board
  nextBoard sqrs = foldl f b sqrs where
    f acc ((Square _ _ p), pt) = putTile acc p (asTile pt)

-- |
createMove :: Board
          ->  Rack
          ->  WordPut
          ->  Dict
          ->  Either String Move
createMove = createMove' standardValidation

-- |
createMove' :: Validator
           ->  Board
           ->  Rack
           ->  WordPut
           ->  Dict
           ->  Either String Move
createMove' validator oldBoard rack wp dict =
  if valid then go else Left errMsg where
    errMsg        = "error: rack missing input letters"
    rackLetters   = fmap letter rack
    valid         = containsAll (toString putLetters) (toString rackLetters)
    putLetters    = letter <$> tiles wp
    rackRemainder = fmap fromLetter $
      foldl' (flip delete) rackLetters putLetters
    go = do (newBoard, score) <- wordPut validator oldBoard wp dict
            return $ Move wp score rackRemainder newBoard
