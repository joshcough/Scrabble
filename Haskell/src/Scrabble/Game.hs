module Scrabble.Game where

import Data.Char (toUpper)
import Data.List (delete,groupBy,intersperse)
import Data.Maybe (catMaybes)
import Debug.Trace
import Scrabble.Bag
import Scrabble.Board
import Scrabble.Commands.AST
import Scrabble.Commands.SExpr
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
  show (Player _ n t s) = concat ["[", n, " tray: ", show t, " score: ", show s, "]"]

newPlayer :: (Name, PlayerType) -> Player
newPlayer (name, typ) = Player typ name [] 0

getNameAndScore :: Player -> (Name, Score)
getNameAndScore (Player _ n _ s) = (n, s)

setTray :: Player -> Tray -> Player
setTray (Player tp n _ s) tr = Player tp n tr s

fillTray :: Player -> Bag -> (Player, Bag)
fillTray (Player tp n currentTray s) bag = (p', b') where
  nrTiles = 7 - length currentTray
  p'      = Player tp n (currentTray ++ take nrTiles bag) s
  b'      = drop nrTiles bag

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

newGame :: [(Name, PlayerType)] -> IO Game
newGame ps = do
  bag  <- newBag
  dict <- dictionary
  let (players,bag') = fillTrays (fmap newPlayer ps) bag
  return $ Game players newBoard bag' dict

fillTrays :: [Player] -> Bag -> ([Player], Bag)
fillTrays ps bag = foldl f ([], bag) ps where
  f (ps,b) p = let (p',b') = fillTray p b in (p':ps,b')

isGameOver :: Game -> Bool
isGameOver (Game players _ bag _) = False -- TODO!

