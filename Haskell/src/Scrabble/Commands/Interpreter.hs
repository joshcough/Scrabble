module Scrabble.Commands.Interpreter where

import Data.Char (toUpper)
import Data.List (delete,foldl',groupBy,intersperse)
import Data.Maybe (catMaybes)
import Scrabble.Bag
import Scrabble.Board
import Scrabble.Commands.AST
import Scrabble.Commands.SExpr
import Scrabble.Game
import Scrabble.Search
import Scrabble.Types
import Prelude hiding (Word)

interpret :: Game -> String -> Either String CommandResult
interpret g cmd = fromString cmd >>= interpretExp g

data Move = Move {
  pointsScored :: Points
 ,remaining :: Tray
 ,boardAfterMove :: ListBoard }

data PrintCommand =
    QueryResult [(Word, Points)]
  | PrintHelp
  | PrintBoard Bool ListBoard
  | PrintScores [(Name,Score)]

data CommandResult = TurnComplete Game | Print PrintCommand

interpretExp :: Game -> ScrabbleExp -> Either String CommandResult
interpretExp g@(Game (p:ps) board bag dict) = f where
  f Skip                    = return . TurnComplete $ nextPlayer g
  f (ShowExp ShowHelp)      = rPrint   PrintHelp
  f (ShowExp (ShowBoard b)) = rPrint $ PrintBoard b board
  f (ShowExp ShowScores)    = rPrint . PrintScores $ getScores g
  f (Search search)         =
    Print . QueryResult <$> interpretSearch search dict
  f (Place pw)              = TurnComplete <$> g' where
    g' = applyMove g <$> interpretPut board (playerTray p) pw
  rPrint = return . Print

getScores :: Game -> [(Name, Score)]
getScores g = getNameAndScore <$> gamePlayers g

interpretSearch :: SearchExp ->
                   Dict      ->
                   Either String [(Word, Points)]
interpretSearch search dict = wps <$> toSearch1 search where
  wps :: Search1 -> [(Word, Points)]
  wps search = fmap f (runSearch1 search dict) where
    f w = (w,simpleWordPoints w)

interpretPut :: ListBoard -> Tray -> PutWord -> Either String Move
interpretPut b tray pw = if valid then go else Left errMsg where
  errMsg        = "error: tray missing input letters"
  trayLetters   = fmap letter tray
  valid         = containsAll putLetters trayLetters
  putLetters    = letter <$> tiles pw
  trayRemainder = fmap fromLetter $
    foldl' (flip delete) trayLetters putLetters
  go = do (newBoard, score) <- putWord b pw
          return $ Move score trayRemainder newBoard

applyMove :: Game -> Move -> Game
applyMove g@(Game (p:ps) _ bag d) (Move pts tray newBoard) =
  Game (ps++[p']) newBoard bag' d where
    (t',bag') = fillTray tray bag
    newScore  = playerScore p + pts
    p' = p {playerTray = t', playerScore = newScore}

