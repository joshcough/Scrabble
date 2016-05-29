{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Scrabble.Move.Scoring where

import Data.List            (foldl')
import Data.Set             (Set)
import Prelude hiding       (Word)
import Scrabble.Bag
import Scrabble.Board.Board

import qualified Data.Set as Set

type Scorer = [Square] -> Board -> Score

noScoring :: Scorer
noScoring _ _ = 0

standardScoring :: Scorer
standardScoring = calculateTurnScore

-- | Calculate the score for ALL words in a turn
calculateTurnScore ::
    [Square] -- ^ all the squares a player placed tiles in this turn
  -> Board   -- ^ the board (with those tiles on it)
  -> Score
calculateTurnScore sqrs board = totalScore where
  totalScore = sum wordScores + bingoBonus
  wordScores = scoreWord (Set.fromList sqrs) <$> words
  words      = Set.toList $ wordsAtPoints (squarePos <$> sqrs) board
  bingoBonus = if length sqrs == 7 then 50 else 0

-- | calculate the score for a single word
scoreWord ::
     Set Square -- ^ the set of squares played in this turn
  -> [Square]   -- ^ one of the words played this turn and the one we are getting the score for
  -> Score
scoreWord playedSquares word =
  base * wordMultiplier * centerMultiplier where

  {- score all the letters -}
  base = sum $ scoreLetter <$> word

  {- the score for a single letter (including its multiplier) -}
  scoreLetter :: Square -> Score
  scoreLetter s@(Square (Just t) bonus _) =
    if Set.member s playedSquares then letterBonus t bonus else score t
  scoreLetter _ = 0

  {- determine the word multipliers for this word
    (based on using any 2W or 3W tiles -}
  wordMultiplier = foldl' f 1 $ filter g word where
    f acc (Square _ W3 _) = acc * 3
    f acc (Square _ W2 _) = acc * 2
    f acc _               = acc * 1
    g s = Set.member s playedSquares

  {- determine the multipliers for a single letter -}
  letterBonus :: Tile -> Bonus -> Score
  letterBonus t L3 = 3 * score t
  letterBonus t L2 = 2 * score t
  letterBonus t _  = score t

  {- if the center square was played, score*2 -}
  centerMultiplier = if centerSquarePlayed then 2 else 1
  centerSquarePlayed = or $ f <$> Set.toList playedSquares where
    f s = squarePos s == centerPosition
