{-# LANGUAGE FlexibleContexts #-}

module Scrabble (
  module Scrabble.Bag
 ,module Scrabble.Board
 ,module Scrabble.Commands.AST
 ,module Scrabble.Commands.Interpreter
 ,module Scrabble.Commands.SExpr
 ,module Scrabble.Dictionary
 ,module Scrabble.Game
 ,module Scrabble.ListBoard
 ,module Scrabble.Matrix
 ,module Scrabble.Play
 ,module Scrabble.Position
 ,module Scrabble.ReplHelpers
 ,start
) where

import Control.Exception (catch, SomeException)
import Control.Monad (when)
import Data.Char (toUpper)
import Data.Maybe (fromJust)
import Prelude hiding (Word)
import Scrabble.Bag
import Scrabble.Board
import Scrabble.Commands.AST
import Scrabble.Commands.Interpreter
import Scrabble.Commands.SExpr
import Scrabble.Dictionary
import Scrabble.Game
import Scrabble.ListBoard
import Scrabble.Matrix
import Scrabble.Play
import Scrabble.Position
import Scrabble.ReplHelpers
import Scrabble.Search
import System.IO.Unsafe

start :: [Player] -> IO ()
start players = do
  g <- newGame players
  printGameBoard g True
  gameLoop g

gameLoop :: (Foldable b, Board b, Vec (Row b)) => Game b -> IO ()
gameLoop g | isGameOver g = gameOver g
gameLoop g = singleTurn g >>= maybe (return ()) gameLoop where

-- Just g means continue with game g.
-- Nothing means game is over.
singleTurn :: (Foldable b, Board b, Vec (Row b)) => Game b -> IO (Maybe (Game b))
singleTurn g =
 (if   isHuman (currentPlayer g)
  then humanTurn False g
  else return (Just $ aiTurn g)) `catch` handleErr where
  handleErr e = putStrLn (show (e :: SomeException)) >> return (Just g)

humanTurn :: (Foldable b, Board b, Vec (Row b)) => Bool -> Game b -> IO (Maybe (Game b))
humanTurn b g = do
  when b $ printGameBoard g True
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
            IO (Maybe (Game b))
applyRes g = ((\(io, g') -> io >> return g') . f g) where
  f :: Board b => Game b -> CommandResult b -> (IO (), Maybe (Game b))
  f _  GameOver                   = (pure (),               Nothing)
  f _ (TurnComplete g)            = (pure (),               Just g)
  f g (Print (QueryResult words)) = (putStrLn $ show words, Just g)
  f g (Print PrintHelp)           = (putStrLn help,         Just g)
  f g (Print (PrintScores scrs))  = (putStrLn $ show scrs,  Just g)
  f g (Print (PrintBoard b brd))  = (printBoard brd b,      Just g)

gameOver :: Board b => Game b -> IO ()
gameOver g = putStrLn ("Game Over!\n" ++ show g)

showHelp :: IO String
showHelp = error "todo"

-- print the board contained in the Game
printGameBoard :: Board b => Game b -> Bool -> IO ()
printGameBoard g = printBoard (gameBoard g)

printBoard :: Board b => b Square -> Bool -> IO ()
printBoard b showBonuses = putStrLn $ showBoard b showBonuses
