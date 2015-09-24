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

interpCommandString :: Game -> String -> Either String CommandResult
interpCommandString g command = fromString command >>= interpretExp g

data Move = Move { pointsScored :: Points, remaining :: Tray, boardAfterMove :: ListBoard }

data PrintCommand =
    QueryResult [(Word, Points)]
  | PrintHelp
  | PrintBoard Bool ListBoard
  | PrintScores [(Name,Score)]

data CommandResult =
    MoveResult Game
  | Print PrintCommand
  | NextPlayer Game

interpretExp :: Game -> ScrabbleExp -> Either String CommandResult
interpretExp g = f where
  f Skip                        = return . NextPlayer $ turnOver g
  f (ShowExp ShowHelp)      = return $ pc   PrintHelp
  f (ShowExp (ShowBoard b)) = return . pc $ PrintBoard b (gameBoard g)
  f (ShowExp ShowScores)    = return . pc . PrintScores $ getScores g
  f (Search search)             =
    pc . QueryResult <$> interpretSearch search (gameDict g)
  f (Place pw)                  = MoveResult  <$> g' where
    g' = applyMove g <$> interpretPut (gameBoard g) t pw
    t  = playerTray $ currentPlayer g
  pc = Print

getScores :: Game -> [(Name, Score)]
getScores g = getNameAndScore <$> gamePlayers g

interpretSearch :: SearchExp -> Dict -> Either String [(Word, Points)]
interpretSearch search dict = lookupWithPoints <$> toSearch1 search where
  lookupWithPoints :: Search1 -> [(Word, Points)]
  lookupWithPoints search = fmap f (runSearch1 search dict) where
    f w = (w,simpleWordPoints w)

interpretPut :: ListBoard -> Tray -> PutWord -> Either String Move
interpretPut b tray pw = if valid then go else Left errMsg where
  errMsg        = "error: tray missing input letters"
  valid         = containsAll putLetters trayLetters
  trayLetters   = fmap letter tray
  putLetters    = letter <$> tiles pw
  trayRemainder = fmap fromLetter $ foldl' (flip delete) trayLetters putLetters
  go = do (newBoard, score) <- putWord b pw
          return $ Move score trayRemainder newBoard

applyMove :: Game -> Move -> Game
applyMove g@(Game (p:ps) _ bag d) (Move points remaining updatedBoard) =
  Game (ps++[player']) updatedBoard bag' d where
    player'   = Player (playerType p) (playerName p) tray' points'
    tray'     = take trayLenth bag ++ remaining
    bag'      = drop trayLenth bag
    points'   = playerScore p + points
    trayLenth = 7 - length remaining
