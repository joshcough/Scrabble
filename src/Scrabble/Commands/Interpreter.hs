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
import Scrabble.Matrix
import Prelude hiding (Word)

interpret :: (Foldable b, Board b, Vec (Row b)) =>
             Game b ->
             String ->
             Either String (CommandResult b)
interpret g cmd = S.fromString cmd >>= interpretExp g

data PrintCommand b =
    QueryResult [(Word, Points)]
  | PrintHelp
  | PrintBoard Bool (b Square)
  | PrintScores [(Name,Score)]

data CommandResult b =
    TurnComplete (Game b)
  | Print (PrintCommand b)
  | GameOver

interpretExp :: (Foldable b, Board b, Vec (Row b)) =>
                Game b      ->
                ScrabbleExp ->
                Either String (CommandResult b)
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
