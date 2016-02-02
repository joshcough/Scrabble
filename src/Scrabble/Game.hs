{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

-- | Player and Game state representation
module Scrabble.Game
  (
    Game(..)
  , Name
  , Player(..)
  , PlayerType(..)
  , ai
  , applyWordPut
  , currentPlayer
  , getScores
  , human
  , isGameOver
  , isHuman
  , newGame
  , nextPlayer
  , printGameBoard
  , putManyWords
  ) where

import Data.Aeson
import Data.Char (toUpper)
import Data.List (intersperse)
import Data.Maybe (fromJust)
import GHC.Generics
import Scrabble.Bag
import Scrabble.Board
import Scrabble.Dictionary
import Scrabble.Move.Move

type Name = String

data PlayerType = Human | AI deriving (Eq, Generic, ToJSON, FromJSON, Show)
data Player = Player {
   playerType  :: PlayerType
 , playerName  :: Name
 , playerRack  :: Rack
 , playerScore :: Score
} deriving (Eq, Generic, ToJSON, FromJSON)

instance Show Player where
  show (Player _ n t s) =
    concat ["[", n, " rack: ", show t, " score: ", show s, "]"]

newPlayer :: (Name, PlayerType) -> Player
newPlayer (name, typ) = Player typ name [] 0

human :: Name -> Player
human name = newPlayer (name, Human)

ai :: Name -> Player
ai name = newPlayer (name, AI)

-- | take tiles from the bag and give them to the player
--   until that player has 7 tiles, or the bag is empty
fillPlayerRack :: Player -> Bag -> (Player, Bag, [Tile])
fillPlayerRack p b = (p', b', removed) where
  (r', b', removed) = fillRack (playerRack p) b
  p' = p { playerRack = r' }

-- | fill the given rack by taking tiles from the bag, and return
--   the newly filled rack
--   the bag with tiles removed
--   the tiles that got removed from the bag
fillRack :: Rack -> Bag -> (Rack, Bag, [Tile])
fillRack r (Bag b) = (r ++ take n b, Bag $ drop n b, take n b) where
  n  = 7 - length r

isHuman :: Player -> Bool
isHuman p = playerType p == Human

{-
What do we need to fully support undo/redo?
Undo:
  * any tiles the player got from the bag at the end
     of the turn must be returned to the bag in order
  * those tiles also must be removed from the players rack.
  * take the tiles the player played off the board
  * removed the points the player got for the turn
  * move player from end of list to start of list
Redo:
  * this is pretty much the same as just applying the move
    so it should be very easy
-}

data Turn = Turn {
   turnPlayer        :: Player  -- ^ The player that made the move.
 , turnWordPut       :: WordPut -- ^ all the tiles laid down.
 , turnPointsScored  :: Points  -- ^ points score in turn.
 , turnRackRemainder :: Rack    -- ^ not yet refilled.
 , turnTilesTaken    :: [Tile]  -- ^ tiles taken from the bag to fill the rack.
} deriving (Eq, Generic, ToJSON, FromJSON)

-- adding this, but not implementing just yet. -JC 2/2/16
data Exchange = Exchange {
   exchangePlayer         :: Player -- ^ The player exchanging tiles.
 , exchangeTilesExchanged :: [Tile] -- ^ The tiles that get put back in the bag.
 , exchangeTilesReceived  :: [Tile] -- ^ The tiles taken from the bag.
 , exchangeOldBag         :: Bag    -- ^ The bag before the exchange.
 , exchangeNewBag         :: Bag    -- ^ The newly shuffled bag after the exchange.
} deriving (Eq, Generic, ToJSON, FromJSON)

data Game = Game {
   gamePlayers :: [Player] -- ^ The players in this game.
 , gameBoard   :: Board    -- ^ The board, possibly with tiles on it.
 , gameBag     :: Bag      -- ^ The bag containing all the remaining tiles.
 , gameDict    :: Dict     -- ^ The official Scrabble English dictionary.
 , gameTurns   :: [Turn]   -- ^ All the turns played in this game.
} deriving (Eq, Generic)

-- | don't serialize the dictionary
instance ToJSON Game where
  toJSON g = object [
    "gamePlayers" .= gamePlayers g,
    "gameBoard"   .= gameBoard   g,
    "gameBag"     .= gameBag     g,
    "gameTurns"   .= gameTurns   g ]

instance FromJSON Game where
  parseJSON = withObject "game" $ \o -> do
    gamePlayers <- o .: "gamePlayers"
    gameBoard   <- o .: "gameBoard"
    gameBag     <- o .: "gameBag"
    let gameDict = dictionaryUnsafe
    gameTurns   <- o .: "gameTurns"
    return Game{..}

newGame :: [Player] -> IO Game
newGame ps = do
  bag  <- newBag
  dict <- Scrabble.Dictionary.dictionary
  let (players,bag') = fillRacks ps bag
  return $ Game (reverse players) newBoard bag' dict [] where
  fillRacks :: [Player] -> Bag -> ([Player], Bag)
  fillRacks ps bag = foldl f ([], bag) ps where
    f (ps,b) p = (p':ps,b') where
      (p',b',_) = fillPlayerRack p b

--deriving instance Eq b => Eq (Game (b Square))

instance Show Game where
  show (Game ps brd bag _ turns) = concat $ intersperse ", "
    ["Game {", show ps, brd', show bag, turns', "}"] where
      brd'   = showBoard True brd
      turns' = "nr turns:" ++ show (length turns)

currentPlayer :: Game -> Player
currentPlayer = head . gamePlayers

nextPlayer :: Game -> Game
nextPlayer g@(Game (p:ps) _ _ _ _) = g { gamePlayers = ps++[p] }
nextPlayer g = g

isGameOver :: Game -> Bool
isGameOver (Game _ _ _ _ _) = False -- TODO!

getScores :: Game -> [(Name, Score)]
getScores g = getNameAndScore <$> gamePlayers g where
  getNameAndScore (Player _ n _ s) = (n, s)

-- | Put a WordPut on the game's board by first creating the move
-- and then simply calling applyMove
applyWordPut :: Game -> WordPut -> Either String Game
applyWordPut g@(Game (p:_) board _ dict _) wp =
  applyMove g <$> createMove board (playerRack p) wp dict

-- | applyMove does all of the following
-- * fills the rack of the player that made the move
-- * updates the score for that player
-- * moves the that player to the end, bringing up the next player.
-- * adds turn to the game's turn list.
applyMove :: Game -> Move -> Game
applyMove g@(Game (p:ps) _ bag d ts) m@(Move wp pts emptyRack newBoard) =
  Game (ps++[p']) newBoard bag' d (turn : ts) where
    -- freshly filled rack, with tiles removed from bag
    (filledRack,bag', tilesRemoved) = fillRack emptyRack bag
    -- update that player by adding the score for the turn
    p' = p { playerRack = filledRack, playerScore = playerScore p + pts }
    turn = Turn p wp pts emptyRack tilesRemoved

-- | print the board contained in the Game
printGameBoard :: Bool -> Game -> IO ()
printGameBoard b = printBoard b . gameBoard

-- | Put words onto an existing board
-- TODO: return all errors: Either [String] (Board,[Score])
putManyWords ::
     Validator
  -> [(String, Orientation, (Int, Int))]
  -> Board
  -> Dict
  -> Either String (Board,[Score])
putManyWords validate words b dict = go (b,[]) wordPuts where
  go :: (Board, [Score]) -> [WordPut] -> Either String (Board, [Score])
  go (b,ss) pws = foldl f (Right (b,ss)) pws where
    f acc wp = do
      (b,scores) <- acc
      (b',score) <- wordPut validate b wp dict
      return (b',scores++[score])

  wordPuts :: [WordPut]
  wordPuts =  (\(s,o,p) -> toWordPut s o p) <$> words where
    toWordPut :: String -> Orientation -> (Int, Int) -> WordPut
    toWordPut w o (x,y) = WordPut putTils where
      coordinates :: [(Int,Int)]
      coordinates = reverse . fst $ foldl f ([],(x,y)) w where
        f (acc,(x,y)) c = ((x,y):acc, adder (x,y))
        adder :: (Int, Int) -> (Int, Int)
        adder = foldOrientation (\(x,y) -> (x+1,y)) (\(x,y) -> (x,y+1)) o
      putTils :: [TilePut]
      putTils = zipWith f w coordinates where
        f c xy = LetterTilePut (fromJust $ tileFromChar (toUpper c)) xy
