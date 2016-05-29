{-# LANGUAGE FlexibleContexts #-}

-- | Main entry point
module Scrabble (
   module Scrabble.Bag
 , module Scrabble.Board.Board
 , module Scrabble.Commands.AST
 , module Scrabble.Commands.Interpreter
 , module Scrabble.Commands.SExpr
 , module Scrabble.Dictionary
 , module Scrabble.Game
 , module Scrabble.Move.Move
 , module Scrabble.Board.Point
 , module Scrabble.ReplHelpers
 , module Scrabble.Board.Square
 , module Scrabble.Tile
 , start
) where

import Control.Exception             (catch, SomeException)
import Control.Monad                 (when)
import Data.List.NonEmpty            (NonEmpty((:|)))
import Prelude                hiding (Word)
import Scrabble.Bag
import Scrabble.Board.Board
import Scrabble.Board.Point
import Scrabble.Commands.AST
import Scrabble.Commands.Interpreter
import Scrabble.Commands.SExpr
import Scrabble.Dictionary
import Scrabble.Game
import Scrabble.Move.Move
import Scrabble.ReplHelpers
import Scrabble.Board.Square
import Scrabble.Tile

start :: [Int -> Player] -> IO ()
start (p:ps) = do
  g <- newGame (p:|ps)
  printGameBoard True g
  gameLoop g
start _ = putStrLn "Error: Game must have at least one player."

gameLoop ::  Game -> IO ()
gameLoop g | isGameOver g = gameOver g
gameLoop g = singleTurn g >>= maybe (return ()) gameLoop where

-- Just g means continue with game g.
-- Nothing means game is over.
singleTurn :: Game -> IO (Maybe Game)
singleTurn g =
 (if   isHuman (currentPlayer g)
  then humanTurn False g
  else return (Just $ aiTurn g)) `catch` handleErr where
  handleErr e = putStrLn (show (e :: SomeException)) >> return (Just g)

humanTurn :: Bool -> Game -> IO (Maybe Game)
humanTurn b g = do
  when b $ printGameBoard True g
  putStrLn $ "Turn for: " ++ show (currentPlayer g)
  putStrLn "Enter command (or type help)"
  command <- getLine
  either (handleErr g) (applyRes g) (interpret g command)
 where handleErr g s = putStrLn s >> humanTurn False g

aiTurn :: Game -> Game
aiTurn _ = error "todo: aiTurn"

applyRes :: Game
         -> CommandResult
         -> IO (Maybe Game)
applyRes g = ((\(io, g') -> io >> return g') . f g) where
  f :: Game -> CommandResult -> (IO (), Maybe Game)
  f _  GameOver                   = (pure (),               Nothing)
  f _ (TurnComplete g)            = (pure (),               Just g)
  f g (Print (QueryResult words)) = (putStrLn $ show words, Just g)
  f g (Print PrintHelp)           = (showHelp,              Just g)
  f g (Print (PrintScores scrs))  = (putStrLn $ show scrs,  Just g)
  f g (Print (PrintBoard b brd))  = (printBoard b brd,      Just g)

gameOver :: Game -> IO ()
gameOver g = putStrLn ("Game Over!\n" ++ show g)

showHelp :: IO ()
showHelp = putStrLn help where
  help :: String
  help = "help unimplemented"
