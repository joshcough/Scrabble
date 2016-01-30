{-# LANGUAGE FlexibleContexts #-}

-- | Interpreter for player input
module Scrabble.Commands.Interpreter where

import Data.List (sort)
import qualified Data.Set as Set
import Scrabble.Bag
import Scrabble.Board
import Scrabble.Dictionary
import Scrabble.Commands.AST
import qualified Scrabble.Commands.SExpr as S
import Scrabble.Game
import Prelude hiding (Word)

interpret :: Game -> String -> Either String CommandResult
interpret g cmd = S.fromString cmd >>= interpretExp g

data PrintCommand =
    QueryResult [(Word, Points)]
  | PrintHelp
  | PrintBoard Bool Board
  | PrintScores [(Name,Score)]

data CommandResult =
    TurnComplete Game
  | Print PrintCommand
  | GameOver

interpretExp :: Game        ->
                ScrabbleExp ->
                Either String CommandResult
interpretExp g@(Game _ board _ dict _) = f where
  f Skip                    = return . TurnComplete $ nextPlayer g
  f Quit                    = return   GameOver
  f (ShowExp ShowHelp)      = return . Print $ PrintHelp
  f (ShowExp (ShowBoard b)) = return . Print $ PrintBoard b board
  f (ShowExp ShowScores)    = return . Print . PrintScores $ getScores g
  f (Search search)         = Print  . QueryResult <$> runSearch search dict
  f (Place wp)              = TurnComplete <$> applyWordPut g wp

runSearch :: SearchExp -> Dict -> Either String [(Word, Points)]
runSearch search dict = f <$> toSearch search where
  f :: (String -> Bool) -> [(Word, Points)]
  f search = g <$> (sort . Set.toList $ Set.filter (search . toString) dict)
  g w = (w, simpleWordPoints w)
