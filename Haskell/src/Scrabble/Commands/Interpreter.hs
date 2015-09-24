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

lookupWithPoints :: Search1 -> Dict -> [(Word, Points)]
lookupWithPoints search dict = fmap (\w -> (w,simpleWordPoints w)) (runSearch1 search dict)

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
  f (ShowCommand ShowHelp)      = return $ pc   PrintHelp
  f (ShowCommand (ShowBoard b)) = return . pc $ PrintBoard b (gameBoard g)
  f (ShowCommand ShowScores)    = return . pc . PrintScores $ getScores g
  f (Search search)             =
    pc . QueryResult <$> interpretSearch search (gameDict g)
  f (Place pw)                  = MoveResult  <$> g' where
    g' = interpMove g <$> interpretPut (gameBoard g) t pw
    t  = playerTray $ currentPlayer g
  pc = Print

getScores :: Game -> [(Name, Score)]
getScores g = getNameAndScore <$> gamePlayers g

interpretSearch :: SearchExp -> Dict -> Either String [(Word, Points)]
interpretSearch search dict = lookupWithPoints <$> toSearch1 search <*> pure dict

interpretPut :: ListBoard -> Tray -> PutWord -> Either String Move
interpretPut b tray pw = if valid then go else Left errMsg where
  errMsg        = "error: tray missing input letters"
  valid         = containsAll putLetters trayLetters
  trayLetters   = fmap letter tray
  putLetters    = letter <$> tiles pw
  trayRemainder = fmap fromLetter $ foldl' (flip delete) trayLetters putLetters
  go = do (newBoard, score) <- putWord b pw
          return $ Move score trayRemainder newBoard

-- TODO: this should probably be Either String Game
interpMove :: Game -> Move -> Game
interpMove g@(Game (p:ps) bd bag d) (Move points remaining updatedBoard) =
  Game (ps++[updatedPlayer]) updatedBoard updatedBag d where
    updatedPlayer = Player (playerType p) (playerName p) updatedTray (playerScore p + points)
    updatedTray   = take (7 - length remaining) bag ++ remaining
    updatedBag    = drop (7 - length remaining) bag
