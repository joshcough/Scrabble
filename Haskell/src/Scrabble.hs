module Scrabble (
  module Scrabble.Bag
 ,module Scrabble.Board
 ,module Scrabble.Commands.AST
 ,module Scrabble.Commands.Interpreter
 ,module Scrabble.Commands.SExpr
 ,module Scrabble.Game
 ,module Scrabble.Search
 ,module Scrabble.Types
) where

import Control.Monad (when)

import Scrabble.Bag
import Scrabble.Board
import Scrabble.Commands.AST
import Scrabble.Commands.Interpreter
import Scrabble.Commands.SExpr
import Scrabble.Game
import Scrabble.Search
import Scrabble.Types

start :: [(Name, PlayerType)] -> IO ()
start players = do
  g <- newGame players
  printListBoard True (gameBoard g)
  gameLoop g

gameLoop :: Game -> IO ()
gameLoop g | isGameOver g = putStrLn ("Game Over!\n" ++ show g)
gameLoop g = singleTurn g >>= gameLoop where
  singleTurn :: Game -> IO Game
  singleTurn g =
    if isHuman (currentPlayer g)
    then humanTurn False g
    else return (aiTurn g)

humanTurn :: Bool -> Game -> IO Game
humanTurn b g = do
  when b (printListBoard True (gameBoard g))
  putStrLn $ "Turn for: " ++ show (currentPlayer g)
  putStrLn "Enter command (or type help)"
  command <- getLine
  either (\s -> putStrLn s >> humanTurn False g)
         (interpCommandRes g)
         (interpCommandString g command)

aiTurn :: Game -> Game
aiTurn g = error "todo: aiTurn"

help = "help unimplemented"

interpCommandRes :: Game -> CommandResult -> IO Game
interpCommandRes _ (MoveResult g)              = return g
interpCommandRes g (Print (QueryResult words)) = putStrLn (show words) >> return g
interpCommandRes g (Print PrintHelp)           = putStrLn help >> return g
interpCommandRes _ (NextPlayer g)              = return g
interpCommandRes g (Print (PrintScores scrs))  = putStrLn (show scrs) >> return g
interpCommandRes g (Print (PrintBoard b brd))  = printListBoard b brd >> return g
