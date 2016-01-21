{-# LANGUAGE StandaloneDeriving #-}

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
  playerRack  :: Rack,
  playerScore :: Score } deriving Eq

instance Show Player where
  show (Player _ n t s) =
    concat ["[", n, " rack: ", show t, " score: ", show s, "]"]

newPlayer :: (Name, PlayerType) -> Player
newPlayer (name, typ) = Player typ name [] 0

human :: Name -> Player
human name = newPlayer (name, Human)

ai :: Name -> Player
ai name = newPlayer (name, AI)

getNameAndScore :: Player -> (Name, Score)
getNameAndScore (Player _ n _ s) = (n, s)

setRack :: Player -> Rack -> Player
setRack p tr = p {playerRack = tr}

getRack :: Player -> Rack
getRack (Player _ _ t _) = t

fillPlayerRack :: Player -> Bag -> (Player, Bag)
fillPlayerRack p b = (p', b') where
  (t', b') = fillRack (playerRack p) b
  p' = p {playerRack = t'}

fillRack :: Rack -> Bag -> (Rack, Bag)
fillRack t bag = (t ++ take n bag, drop n bag) where
  n  = 7 - length t

isHuman :: Player -> Bool
isHuman p = playerType p == Human

data Game b = Game {
  gamePlayers :: [Player],
  gameBoard   :: b Square,
  gameBag     :: Bag,
  gameDict    :: Dict }

--deriving instance Eq b => Eq (Game (b Square))

instance Board b => Show (Game b) where
  show (Game ps brd bag dict) = concat $
    intersperse ", " ["Game {", show ps, brd', show bag, "}"] where
      brd' = showBoard True brd

currentPlayer :: Board b => Game b -> Player
currentPlayer = head . gamePlayers

nextPlayer :: Board b => Game b  -> Game b
nextPlayer g@(Game (p:ps) _ _ _) = g { gamePlayers = ps++[p] }

fillRacks :: [Player] -> Bag -> ([Player], Bag)
fillRacks ps bag = foldl f ([], bag) ps where
  f (ps,b) p = (p':ps,b') where
    (p',b') = fillPlayerRack p b

isGameOver :: Board b => Game b -> Bool
isGameOver (Game players _ bag _) = False -- TODO!
