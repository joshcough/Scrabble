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
data CommandResult =
 MoveResult Game              |
 QueryResult [(Word, Points)] |
 ShowHelp                     |
 PrintScores [(Name,Score)]   |
 NextPlayer Game              |
 PrintBoard Bool ListBoard

interpretExp :: Game -> ScrabbleExp -> Either String CommandResult
interpretExp g Skip            = return . NextPlayer $ turnOver g
interpretExp _ Help            = return ShowHelp
interpretExp g ShowScores      = return . PrintScores $ getScores g
interpretExp g (ShowBoard b)   = return $ PrintBoard b (gameBoard g)
interpretExp g (Search search) = QueryResult <$> interpretSearch search (gameDict g)
interpretExp g (Place pw)      = MoveResult  <$> g' where
  g' = interpMove g <$> interpretPut (gameBoard g) (playerTray (currentPlayer g)) pw

getScores :: Game -> [(Name, Score)]
getScores g = getNameAndScore <$> gamePlayers g

interpretSearch :: SearchExp -> Dict -> Either String [(Word, Points)]
interpretSearch search dict = lookupWithPoints <$> toSearch1 search <*> pure dict

interpretPut :: ListBoard -> Tray -> PutWord -> Either String Move
interpretPut b tray pw = if valid then return move else Left errMsg where
  move              = Move score trayRemainder newBoard
  errMsg            = "error: tray missing input letters"
  valid             = containsAll putLetters trayLetters
  (newBoard, score) = putWord b pw
  trayLetters       = fmap letter tray
  putLetters        = filter (\c -> c /= '@') . (fmap (toUpper . letter)) . catMaybes $ (tiles._putWordTiles) pw
  trayRemainder     = fmap fromLetter $ foldl' (flip delete) trayLetters putLetters

-- TODO: this should probably be Either String Game
interpMove :: Game -> Move -> Game
interpMove g@(Game (p:ps) bd bag d) (Move points remaining updatedBoard) =
  Game (ps++[updatedPlayer]) updatedBoard updatedBag d where
    updatedPlayer = Player (playerType p) (playerName p) updatedTray (playerScore p + points)
    updatedTray   = take (7 - length remaining) bag ++ remaining
    updatedBag    = drop (7 - length remaining) bag
