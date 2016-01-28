{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Scrabble.Move.Move (
  module Scrabble.Move.WordPut
 ,Move(..)
 ,createMove
 ,wordPut
) where

import Data.List (delete, foldl')
import Prelude hiding (Word)
import Scrabble.Bag
import Scrabble.Board
import Scrabble.Dictionary
import Scrabble.Matrix
import Scrabble.Move.Scoring
import Scrabble.Move.Validation
import Scrabble.Move.WordPut
import Scrabble.Position
import Scrabble.Search (containsAll)

data Move b = Move {
  moveWordPut         :: WordPut
 ,movePointsScored    :: Points
 ,moveRackAfter       :: Rack -- not yet refilled
 ,moveBoardAfterMove  :: b Square }

-- | Attempt to lay tiles on the board.
--   Validate the entire move.
--   Calculate the score of the move. -}
wordPut :: (Foldable b, Board b, Vec (Row b))  =>
           b Square ->
           WordPut  ->
           Dict     -> -- the dictionary
           Either String (b Square, Score)
wordPut b wp dict = do
  squares <- squaresPlayedThisTurn
  let b' = nextBoard $ zip squares (tiles wp)
  validateMove (zipSquaresAndTiles squares) b b' dict
  return (b', calculateTurnScore squares b') where

  squaresPlayedThisTurn :: Either String [Square]
  squaresPlayedThisTurn = traverse f (pos <$> tiles wp) where
    f :: Position -> Either String Square
    f p = maybe (Left $ "out of bounds: " ++ show p) Right $ elemAt b p

  zipSquaresAndTiles :: [Square] -> [(Square,TilePut,Position)]
  zipSquaresAndTiles sqrs = zipWith f sqrs (tiles wp) where
    f s t = (s, t, pos t)

  --TODO: the compiler won't let me put this type sig here.
  --      even with ScopedTypeVariables.
  --nextBoard :: [(Square,TilePut)] -> b Square
  nextBoard sqrs = foldl f b sqrs where
    f acc ((Square _ _ p), pt) = putTile acc p (asTile pt)

createMove :: (Foldable b, Board b, Vec (Row b)) =>
               b Square ->
               Rack     ->
               WordPut  ->
               Dict     ->
               Either String (Move b)
createMove oldBoard rack wp dict =
  if valid then go else Left errMsg where
    errMsg        = "error: rack missing input letters"
    rackLetters   = fmap letter rack
    valid         = containsAll (toString putLetters) (toString rackLetters)
    putLetters    = letter <$> tiles wp
    rackRemainder = fmap fromLetter $
      foldl' (flip delete) rackLetters putLetters
    go = do (newBoard, score) <- wordPut oldBoard wp dict
            return $ Move wp score rackRemainder newBoard
