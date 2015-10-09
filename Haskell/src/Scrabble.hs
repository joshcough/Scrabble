{-# LANGUAGE FlexibleContexts #-}

module Scrabble (
  module Scrabble.Bag
 ,module Scrabble.Board
 ,module Scrabble.Commands.AST
 ,module Scrabble.Commands.Interpreter
 ,module Scrabble.Commands.SExpr
 ,module Scrabble.Game
 ,module Scrabble.ListBoard
 ,module Scrabble.Matrix
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
import Scrabble.ListBoard
import Scrabble.Matrix
import Scrabble.Search
import Scrabble.Types
import System.IO.Unsafe

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

{- test putting some words on a brand new board -}
quickPut :: [(String, Orientation, (Int, Int))] ->
            (ListBoard,[Score])
quickPut words = unsafePerformIO $ do
  dict <- dictionary
  let e = quickPut' words (newBoard :: ListBoard) dict
  return $ either error id e

{- test put some words onto an existing board -}
quickPut' :: (Foldable b, Board b, Vec (Row b)) =>
             [(String, Orientation, (Int, Int))] ->
             b Square ->
             Dict     ->
             Either String (b Square,[Score])
quickPut' words b dict = go (b,[]) putWords where

  {- TODO: this is pretty awful
     I think EitherT over State could clean it up,
     but not sure if i want to do that.
     Also, I can't put the type here, again.
  -}
  --go :: (b Square, [Score]) -> [PutWord] -> Either String (b Square, [Score])
  go (b,ss) pws = foldl f (Right (b,ss)) pws where
    f acc pw = do
      (b,scores) <- acc
      (b',score) <- putWord b pw dict
      return (b',scores++[score])

  putWords :: [PutWord]
  putWords =  (\(s,o,p) -> toPutWord s o p) <$> words where
    toPutWord :: String -> Orientation -> (Int, Int) -> PutWord
    toPutWord w o (x,y) = PutWord putTils where
      adder :: (Int, Int) -> (Int, Int)
      adder = catOrientation (\(x,y) -> (x+1,y)) (\(x,y) -> (x,y+1)) o
      coordinates :: [(Int,Int)]
      coordinates = reverse . fst $ foldl f ([],(x,y)) w where
        f (acc,(x,y)) c = ((x,y):acc, adder (x,y))
      putTils :: [PutTile]
      putTils = zipWith f w coordinates where
        f c xy = PutLetterTile (mkTile c) (pos xy)
