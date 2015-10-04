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

start :: [Player] -> IO ()
start players = do
  g <- newGame players
  printListBoard True (gameBoard g)
  gameLoop g

gameLoop :: Game -> IO ()
gameLoop g | isGameOver g = putStrLn ("Game Over!\n" ++ show g)
gameLoop g = singleTurn g >>= gameLoop where
  singleTurn :: Game -> IO Game
  singleTurn g =
    if   isHuman (currentPlayer g)
    then humanTurn False g
    else return (aiTurn g)

humanTurn :: Bool -> Game -> IO Game
humanTurn b g = do
  when b (printListBoard True $ gameBoard g)
  putStrLn $ "Turn for: " ++ show (currentPlayer g)
  putStrLn "Enter command (or type help)"
  command <- getLine
  either (handleErr g) (applyRes g) (interpret g command)

handleErr :: Game -> String -> IO Game
handleErr g s = putStrLn s >> humanTurn False g

aiTurn :: Game -> Game
aiTurn g = error "todo: aiTurn"

help = "help unimplemented"

applyRes :: Game -> CommandResult -> IO Game
applyRes g = (go . f g) where
  go (io, g) = io >> return g
  f :: Game -> CommandResult -> (IO (), Game)
  f _ (TurnComplete g)            = (pure (),               g)
  f g (Print (QueryResult words)) = (putStrLn $ show words, g)
  f g (Print PrintHelp)           = (putStrLn help,         g)
  f g (Print (PrintScores scrs))  = (putStrLn $ show scrs,  g)
  f g (Print (PrintBoard b brd))  = (printListBoard b brd,  g)
