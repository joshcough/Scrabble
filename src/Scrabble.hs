module Scrabble (
  module Scrabble.Bag
 ,module Scrabble.Board
 ,module Scrabble.Search
 ,module Scrabble.Types
) where

import Scrabble.Bag
import Scrabble.Board
import Scrabble.Search
import Scrabble.Types

type Name  = String
type Score = Int

data Player = Human Name Tray Score | AI Name Tray Score deriving Show
data Game = Game { players :: [Player], board :: Board, bag :: Bag } deriving Show

newGame :: [Player] -> IO Game
newGame players = Game players newBoard <$> newBag

isGameOver :: Game -> Bool
isGameOver (Game players _ bag) = False

start :: [Player] -> IO ()
start players = newGame players >>= gameLoop (cycle players)

gameLoop :: [Player] -> Game -> IO ()
gameLoop _ g | isGameOver g = putStrLn ("Game Over!\n" ++ show g)
gameLoop (p:ps) g = singleTurn p g >>= gameLoop ps

singleTurn :: Player -> Game  -> IO Game
singleTurn h@(Human _ _ _) = humanTurn h
singleTurn ai = return . aiTurn ai

humanTurn :: Player -> Game -> IO Game
humanTurn h@(Human name tray score) (Game ps board bag) = do
  printBoard board
  printBoardClean board
  putStrLn $ "Turn for: " ++ show h
  putStrLn "Enter x"
  x <- getLine
  putStrLn "Enter y"
  y <- getLine
  putStrLn "Enter H/V"
  o <- getLine
  putStrLn "Enter word"
  word <- getLine
  let next = placeWord (read x, read y) (if o == "H" then Horizontal else Vertical) word board
  return (Game ps next bag)
humanTurn _ _ = error "impossible"

aiTurn :: Player -> Game -> Game
aiTurn ai@(AI name tray score) g = error "todo"
aiTurn p g = error "impossible"


