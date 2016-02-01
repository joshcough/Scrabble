{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Scrabble.Move.Move (
  module Scrabble.Move.WordPut
 ,module Scrabble.Move.Validation
 ,Move(..)
 ,createMove
 ,wordPut
) where

import Data.Aeson (ToJSON, FromJSON)
import Data.List (delete, foldl')
import GHC.Generics
import Prelude hiding (Word)
import Scrabble.Bag
import Scrabble.Board
import Scrabble.Dictionary
import Scrabble.Move.Scoring
import Scrabble.Move.Validation
import Scrabble.Move.WordPut
import Scrabble.Search (containsAll)

data Move = Move {
  moveWordPut         :: WordPut -- ^ all the tiles laid down
 ,movePointsScored    :: Points  -- ^ points score in turn
 ,moveRackAfter       :: Rack    -- ^ not yet refilled
 ,moveBoardAfterMove  :: Board   -- ^ the state of the board after the move
} deriving (Eq, Generic, ToJSON, FromJSON)

-- | Attempt to lay tiles on the board.
--   Validate the entire move.
--   Calculate the score of the move.
wordPut :: Validator -- ^ validation algo
       ->  Board     -- ^ the board to put the word on
       ->  WordPut   -- ^ the word being put on the board
       ->  Dict      -- ^ the scrabble dictionary
       ->  Either String (Board, Score)
wordPut validate b wp dict = do
  squares <- squaresPlayedThisTurn
  let b' = nextBoard $ wordPutTiles wp
  validate (wordPutTiles wp) b b' dict
  return (b', calculateTurnScore squares b') where

    squaresPlayedThisTurn :: Either String [Square]
    squaresPlayedThisTurn = traverse f (tilePutPoint <$> wordPutTiles wp) where
      f p = maybe (Left $ "out of bounds: " ++ show p) Right $ elemAt b p

    nextBoard :: [TilePut] -> Board
    nextBoard ts = putTiles b $ (\tp -> (tilePutPoint tp, asTile tp)) <$> ts

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
createMove' validate oldBoard rack wp dict =
  if valid then go else Left errMsg where
    errMsg        = "error: rack missing input letters"
    rackLetters   = fmap letter rack
    valid         = containsAll (toString putLetters) (toString rackLetters)
    putLetters    = letter <$> wordPutTiles wp
    rackRemainder = fmap fromLetter $
      foldl' (flip delete) rackLetters putLetters
    go = do (newBoard, score) <- wordPut validate oldBoard wp dict
            return $ Move wp score rackRemainder newBoard
