{-# LANGUAGE FlexibleContexts #-}
module Scrabble.Commands.Interpreter where

import Data.Char (toUpper)
import Data.List (delete,foldl',groupBy,intersperse,sort)
import Data.Maybe (catMaybes, fromJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Scrabble.Bag
import Scrabble.Board
import Scrabble.Dictionary
import Scrabble.Commands.AST
import qualified Scrabble.Commands.SExpr as S
import Scrabble.Game
import Scrabble.Matrix
import Scrabble.Play
import Scrabble.Search (containsAll)
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
interpretExp g@(Game (p:ps) board bag dict) = f where
  f Skip                    = return . TurnComplete $ nextPlayer g
  f Quit                    = return   GameOver
  f (ShowExp ShowHelp)      = rPrint   PrintHelp
  f (ShowExp (ShowBoard b)) = rPrint $ PrintBoard b board
  f (ShowExp ShowScores)    = rPrint . PrintScores $ getScores g
  f (Search search)         =
    Print . QueryResult <$> interpretSearch search dict
  f (Place pw)              = TurnComplete <$> applyPutWord g pw
  rPrint = return . Print

interpretSearch :: SearchExp ->
                   Dict      ->
                   Either String [(Word, Points)]
interpretSearch search dict = f <$> toSearch search where
  f :: (String -> Bool) -> [(Word, Points)]
  f search = g <$> (sort . Set.toList $ Set.filter (search . toString) dict)
  g w = (w,simpleWordPoints w)
