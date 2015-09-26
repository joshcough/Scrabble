module Scrabble.Game where

import Data.Char (toUpper)
import Data.List (delete,groupBy,intersperse)
import Data.Maybe (catMaybes)
import Debug.Trace
import Scrabble.Bag
import Scrabble.Board
import Scrabble.Search
import Scrabble.Types
import Prelude hiding (Word)

type Name = String

data PlayerType = Human | AI deriving (Eq, Show)
data Player = Player {
  playerType  :: PlayerType,
  playerName  :: Name,
  playerTray  :: Tray,
  playerScore :: Score } deriving Eq

instance Show Player where
  show (Player _ n t s) =
    concat ["[", n, " tray: ", show t, " score: ", show s, "]"]

newPlayer :: (Name, PlayerType) -> Player
newPlayer (name, typ) = Player typ name [] 0

human :: Name -> Player
human name = newPlayer (name, Human)

ai :: Name -> Player
ai name = newPlayer (name, AI)

getNameAndScore :: Player -> (Name, Score)
getNameAndScore (Player _ n _ s) = (n, s)

setTray :: Player -> Tray -> Player
setTray p tr = p {playerTray = tr}

getTray :: Player -> Tray
getTray (Player _ _ t _) = t

fillPlayerTray :: Player -> Bag -> (Player, Bag)
fillPlayerTray p b = (p', b') where
  (t', b') = fillTray (playerTray p) b
  p' = p {playerTray = t'}

fillTray :: Tray -> Bag -> (Tray, Bag)
fillTray t bag = (t ++ take n bag, drop n bag) where
  n  = 7 - length t

isHuman :: Player -> Bool
isHuman p = playerType p == Human

data Game = Game {
  gamePlayers :: [Player],
  gameBoard   :: ListBoard,
  gameBag     :: Bag,
  gameDict    :: Dict } deriving Eq

instance Show Game where
  show (Game ps brd bag dict) = concat $
    intersperse ", " ["Game {", show ps, brd', show bag, "}"] where
      brd' = displayBoard brd

currentPlayer :: Game -> Player
currentPlayer = head . gamePlayers

nextPlayer :: Game -> Game
nextPlayer g@(Game (p:ps) _ _ _) = g { gamePlayers = ps++[p] }

newGame :: [Player] -> IO Game
newGame ps = do
  bag  <- newBag
  dict <- dictionary
  let (players,bag') = fillTrays ps bag
  return $ Game (reverse players) newBoard bag' dict

fillTrays :: [Player] -> Bag -> ([Player], Bag)
fillTrays ps bag = foldl f ([], bag) ps where
  f (ps,b) p = (p':ps,b') where
    (p',b') = fillPlayerTray p b

isGameOver :: Game -> Bool
isGameOver (Game players _ bag _) = False -- TODO!

