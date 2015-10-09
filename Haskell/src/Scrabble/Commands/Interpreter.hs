{-# LANGUAGE FlexibleContexts #-}

module Scrabble.Commands.Interpreter where

import Data.Char (toUpper)
import Data.List (delete,foldl',groupBy,intersperse)
import Data.Maybe (catMaybes)
import Scrabble.Bag
import Scrabble.Board
import Scrabble.Commands.AST
import Scrabble.Commands.SExpr
import Scrabble.Game
import Scrabble.Matrix
import Scrabble.Search
import Scrabble.Types
import Prelude hiding (Word)

interpret :: (Foldable b, Board b, Vec (Row b)) =>
             Game b ->
             String ->
             Either String (CommandResult b)
interpret g cmd = fromString cmd >>= interpretExp g

data Move b = Move {
  pointsScored :: Points
 ,remaining :: Rack
 ,boardAfterMove :: b Square }

data PrintCommand b =
    QueryResult [(Word, Points)]
  | PrintHelp
  | PrintBoard Bool (b Square)
  | PrintScores [(Name,Score)]

data CommandResult b =
    TurnComplete (Game b)
  | Print (PrintCommand b)

interpretExp :: (Foldable b, Board b, Vec (Row b)) =>
                Game b      ->
                ScrabbleExp ->
                Either String (CommandResult b)
interpretExp g@(Game (p:ps) board bag dict) = f where
  f Skip                    = return . TurnComplete $ nextPlayer g
  f (ShowExp ShowHelp)      = rPrint   PrintHelp
  f (ShowExp (ShowBoard b)) = rPrint $ PrintBoard b board
  f (ShowExp ShowScores)    = rPrint . PrintScores $ getScores g
  f (Search search)         =
    Print . QueryResult <$> interpretSearch search dict
  f (Place pw)              = TurnComplete <$> g' where
    g' = applyMove g <$> interpretPut board (playerRack p) pw dict
  rPrint = return . Print

getScores :: Game a -> [(Name, Score)]
getScores g = getNameAndScore <$> gamePlayers g

interpretSearch :: SearchExp ->
                   Dict      ->
                   Either String [(Word, Points)]
interpretSearch search dict = wps <$> toSearch1 search where
  wps :: Search1 -> [(Word, Points)]
  wps search = fmap f (runSearch1 search dict) where
    f w = (w,simpleWordPoints w)

interpretPut :: (Foldable b, Board b, Vec (Row b)) =>
                b Square ->
                Rack     ->
                PutWord  ->
                Dict     ->
                Either String (Move b)
interpretPut b rack pw dict =
  if valid then go else Left errMsg where
    errMsg        = "error: rack missing input letters"
    rackLetters   = fmap letter rack
    valid         = containsAll putLetters rackLetters
    putLetters    = letter <$> tiles pw
    rackRemainder = fmap fromLetter $
      foldl' (flip delete) rackLetters putLetters
    go = do (newBoard, score) <- putWord b pw dict
            return $ Move score rackRemainder newBoard

applyMove :: Board b => Game b -> Move b -> Game b
applyMove g@(Game (p:ps) _ bag d) (Move pts rack newBoard) =
  Game (ps++[p']) newBoard bag' d where
    (t',bag') = fillRack rack bag
    newScore  = playerScore p + pts
    p' = p {playerRack = t', playerScore = newScore}

