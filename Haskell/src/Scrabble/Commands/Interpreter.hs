module Scrabble.Commands.Interpreter where

import Data.Char (toUpper)
import Data.List (delete,groupBy,intersperse)
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
interpCommandString g command =
  fromString command >>= interpretExp (gameBoard g) (playerTray (nextPlayer g)) (gameDict g)

lookupWithPoints :: Search1 -> Dict -> [(Word, Points)]
lookupWithPoints search dict = fmap (\w -> (w,simpleWordPoints w)) (runSearch1 search dict)

data Move = Move { pointsScored :: Points, remaining :: Tray, boardAfterMove :: ListBoard }
data CommandResult =
 MoveResult Move              |
 QueryResult [(Word, Points)] |
 ShowHelp                     |
 NextPlayer                   |
 PrintBoard Bool

-- TODO: this should probably be Either String Game
interpMove :: Game -> Move -> Game
interpMove g@(Game (p:ps) bd bag d) (Move points remaining updatedBoard) =
  Game (ps++[updatedPlayer]) updatedBoard updatedBag d where
    updatedPlayer = Player (playerType p) (playerName p) updatedTray (playerScore p + points)
    updatedTray   = take (7 - length remaining) bag ++ remaining
    updatedBag    = drop (7 - length remaining) bag

interpretExp :: ListBoard -> Tray -> Dict -> ScrabbleExp -> Either String CommandResult
interpretExp _ _ _ Skip = return NextPlayer
interpretExp _ _ _ Help = return ShowHelp
interpretExp _ _ _ (ShowBoard b)      = return $ PrintBoard b
interpretExp _ _ dict (Search search) = QueryResult <$> interpretSearch search dict
interpretExp b t _ (Place pw) = MoveResult <$> interpretPut b t pw

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
  trayRemainder     = fmap fromLetter $ foldl (flip delete) trayLetters putLetters

