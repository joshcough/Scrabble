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

import Scrabble.Bag
import Scrabble.Board
import Scrabble.Commands.AST
import Scrabble.Commands.Interpreter
import Scrabble.Commands.SExpr
import Scrabble.Game
import Scrabble.Search
import Scrabble.Types

start :: [(Name, PlayerType)] -> IO ()
start players = newGame players >>= gameLoop

gameLoop :: Game -> IO ()
gameLoop g | isGameOver g = putStrLn ("Game Over!\n" ++ show g)
gameLoop g = singleTurn g >>= gameLoop

singleTurn :: Game -> IO Game
singleTurn g =
  if isHuman (currentPlayer g)
  then humanTurn g
  else return (aiTurn g)

humanTurn :: Game -> IO Game
humanTurn g = do
  printListBoard True (gameBoard g)
  putStrLn $ "Turn for: " ++ show (currentPlayer g)
  putStrLn "Enter command (or type help)"
  command <- getLine
  either (\s -> putStrLn s >> humanTurn g)
         (interpCommandRes g)
         (interpCommandString g command)

aiTurn :: Game -> Game
aiTurn g = error "todo: aiTurn"

interpCommandRes :: Game -> CommandResult -> IO Game
interpCommandRes _ (MoveResult g)      = return g
interpCommandRes g (QueryResult words) = putStrLn (show words) >> return g
interpCommandRes g ShowHelp            = putStrLn "help unimplemented" >> return g
interpCommandRes _ (NextPlayer g)      = return g
interpCommandRes g (PrintScores scrs)  = putStrLn (show scrs) >> return g
interpCommandRes g (PrintBoard b brd)  = printListBoard b brd >> return g

