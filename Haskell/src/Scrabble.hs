{-# LANGUAGE FlexibleContexts #-}

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
import Scrabble.ListBoard ()
import Scrabble.Matrix
import Scrabble.Search
import Scrabble.Types

start :: [Player] -> IO ()
start players = do
  g <- newGame players
  printBoard True (gameBoard g)
  gameLoop g

newGame :: [Player] -> IO (Game ListMatrix)
newGame ps = do
  bag  <- newBag
  dict <- dictionary
  let (players,bag') = fillTrays ps bag
  return $ Game (reverse players) newBoard bag' dict

gameLoop :: (Foldable b, Board b, Vec (Row b)) => Game b -> IO ()
gameLoop g | isGameOver g = gameOver g
gameLoop g = singleTurn >>= gameLoop where
  singleTurn =
    if   isHuman (currentPlayer g)
    then humanTurn False g
    else return (aiTurn g)

humanTurn :: (Foldable b, Board b, Vec (Row b)) => Bool -> Game b -> IO (Game b)
humanTurn b g = do
  when b (printBoard True $ gameBoard g)
  putStrLn $ "Turn for: " ++ show (currentPlayer g)
  putStrLn "Enter command (or type help)"
  command <- getLine
  either (handleErr g) (applyRes g) (interpret g command)
 where handleErr g s = putStrLn s >> humanTurn False g

aiTurn :: Board b => Game b -> Game b
aiTurn g = error "todo: aiTurn"

help = "help unimplemented"

applyRes :: Board b =>
            Game b          ->
            CommandResult b ->
            IO (Game b)
applyRes g = ((\(io, g') -> io >> return g') . f g) where
  f :: Board b => Game b -> CommandResult b -> (IO (), Game b)
  f _ (TurnComplete g)            = (pure (),               g)
  f g (Print (QueryResult words)) = (putStrLn $ show words, g)
  f g (Print PrintHelp)           = (putStrLn help,         g)
  f g (Print (PrintScores scrs))  = (putStrLn $ show scrs,  g)
  f g (Print (PrintBoard b brd))  = (printBoard b brd,      g)

gameOver :: Board b => Game b -> IO ()
gameOver g = putStrLn ("Game Over!\n" ++ show g)
