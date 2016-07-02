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
  , Turn(..)
  , ai
  , applyWord
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
import Data.Char            (toUpper)
import Data.List            (intersperse)
import Data.List.NonEmpty   (NonEmpty((:|)), (<|))
import GHC.Generics
import Scrabble.Bag
import Scrabble.Board.Board
import Scrabble.Dictionary
import Scrabble.Move.Move

import qualified Data.List.NonEmpty as NE

type Name = String

-- | Players are either human or AI. Unfortunately, dolphins are not supported.
data PlayerType = Human | AI deriving (Eq, Generic, ToJSON, FromJSON, Show)

-- | A player, human or AI.
data Player = Player {
   playerType  :: PlayerType -- ^ The type of player (human or AI)
 , playerId    :: Int        -- ^ Id necessary in case of duplicate names.
 , playerName  :: Name       -- ^ Player name
 , playerRack  :: Rack       -- ^ The players rack (containing tiles)
 , playerScore :: Score      -- ^ Players current score in the game.
} deriving (Eq, Generic, ToJSON, FromJSON)

instance Show Player where
  show (Player _ pid n t s) = concat [
    "[", n, " rack: ", show t, " score: ", show s, "id: ", show pid, "]"]

newPlayer :: (Name, PlayerType) -> Int -> Player
newPlayer (name, typ) = Player typ 0 name newRack

human :: Name -> Int -> Player
human name = newPlayer (name, Human)

ai :: Name -> Int -> Player
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
fillRack (Rack r) (Bag b) =
  (Rack $ r ++ take n b, Bag $ drop n b, take n b) where
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
   turnPlayerId      :: Int     -- ^ The id of the player that made the move.
 , turnWordPut       :: WordPut -- ^ all the tiles laid down.
 , turnPointsScored  :: Points  -- ^ points score in turn.
 , turnRackRemainder :: Rack    -- ^ not yet refilled.
 , turnTilesTaken    :: [Tile]  -- ^ tiles taken from the bag to fill the rack.
} deriving (Eq, Generic, Show)

-- | don't serialize the dictionary
instance ToJSON Turn where
  toJSON t = object [
    "playerId"          .= turnPlayerId      t,
    "tilesPlayed"       .= turnWordPut       t,
    "points"            .= turnPointsScored  t,
    "rackRemainder"     .= turnRackRemainder t,
    "tilesTakenFromBag" .= (tilesToJSON $ turnTilesTaken t) ]

instance FromJSON Turn where
  parseJSON = withObject "turn" $ \o -> do
    turnPlayerId      <- o .: "playerId"
    turnWordPut       <- o .: "tilesPlayed"
    turnPointsScored  <- o .: "points"
    turnRackRemainder <- o .: "rackRemainder"
    turnTilesTaken'   <- o .: "tilesTakenFromBag"
    turnTilesTaken    <- tilesFromJSON "Turn tiles" ("invalid tile in turn: " ++) id turnTilesTaken'
    return Turn{..}

{-
-- adding this, but not implementing just yet. -JC 2/2/16
data Exchange = Exchange {
   exchangePlayer         :: Player -- ^ The player exchanging tiles.
 , exchangeTilesExchanged :: [Tile] -- ^ The tiles that get put back in the bag.
 , exchangeTilesReceived  :: [Tile] -- ^ The tiles taken from the bag.
 , exchangeOldBag         :: Bag    -- ^ The bag before the exchange.
 , exchangeNewBag         :: Bag    -- ^ The newly shuffled bag after the exchange.
} deriving (Eq, Generic, ToJSON, FromJSON)
-}

data Game = Game {
   gamePlayers :: NonEmpty Player -- ^ The players in this game.
 , gameBoard   :: Board Square    -- ^ The board, possibly with tiles on it.
 , gameBag     :: Bag             -- ^ The bag containing all the remaining tiles.
 , gameDict    :: Dict            -- ^ The official Scrabble English dictionary.
 , gameTurns   :: [Turn]          -- ^ All the turns played in this game.
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
    let gameDict = unsafeReadEnglishDictionary
    gameTurns   <- o .: "gameTurns"
    return Game{..}

-- TODO: Pass in a language.
newGame :: NonEmpty (Int -> Player) -> IO Game
newGame ps = do
  let players = (\(f,i) -> f i) <$> NE.zip ps (NE.fromList [0..])
  bag  <- newBag
  dict <- Scrabble.Dictionary.englishDictionary
  let (players',bag') = filledRacks players bag
  return $ Game (NE.reverse players') newBoard bag' dict [] where
  filledRacks :: NonEmpty Player -> Bag -> (NonEmpty Player, Bag)
  filledRacks (p1:|players) bag = foldl f (g p1) players where
    f (acc,b) a = let (p',b',_) = fillPlayerRack a b in (p' <| acc, b')
    g a = let (p',b',_) = fillPlayerRack a bag in (p':|[], b')

instance Show Game where
  show (Game ps brd bag _ turns) = concat $ intersperse ", "
    ["Game {", show ps, brd', show bag, turns', "}"] where
      brd'   = showBoard True brd
      turns' = "nr turns:" ++ show (length turns)

currentPlayer :: Game -> Player
currentPlayer = NE.head . gamePlayers

nextPlayer :: Game -> Game
nextPlayer g@(Game (p:|(p2:ps)) _ _ _ _) = g { gamePlayers = p2:|(ps++[p]) }
nextPlayer g = g

isGameOver :: Game -> Bool
isGameOver (Game _ _ _ _ _) = False -- TODO!

-- | TODO: use the player id if overlapping names.
getScores :: Game -> NonEmpty (Name, Score)
getScores g = getNameAndScore <$> gamePlayers g where
  getNameAndScore (Player _ _ n _ s) = (n, s)

-- | Put a WordPut on the game's board by first creating the move
-- and then simply calling applyMove
applyWordPut :: Game -> WordPut -> Either String Game
applyWordPut g@(Game (p:|_) board _ dict _) wp =
  applyMove g <$> createMove board (playerRack p) wp dict

-- | Add a word to the board
applyWord :: String
         ->  Orientation
         ->  Point
         ->  [Char]
         ->  Game
         ->  Either String Game
applyWord s o p blanks g = makeWordPut s o p blanks >>= applyWordPut g

-- | applyMove does all of the following
-- * fills the rack of the player that made the move
-- * updates the score for that player
-- * moves the that player to the end, bringing up the next player.
-- * adds turn to the game's turn list.
applyMove :: Game -> Move -> Game
applyMove (Game (p:|ps) _ bag d ts) (Move wp pts emptyRack newBoard) =
  Game (NE.fromList $ ps++[p']) newBoard bag' d (turn : ts) where
    -- freshly filled rack, with tiles removed from bag
    (filledRack,bag', tilesRemoved) = fillRack emptyRack bag
    -- update that player by adding the score for the turn
    p' = p { playerRack = filledRack, playerScore = playerScore p + pts }
    turn = Turn (playerId p) wp pts emptyRack tilesRemoved

-- | print the board contained in the Game
printGameBoard :: Bool -> Game -> IO ()
printGameBoard b = printBoard b . gameBoard

-- | Put words onto an existing board
-- TODO: return all errors: Either [String] (Board,[Score])
putManyWords ::
     Validator
  -> [(String, Orientation, Point)]
  -> Board Square
  -> Dict
  -> Either String (Board Square, [Score])
putManyWords validate words b dict = wordPuts >>= go (b,[]) where
  -- put all the words on the board
  go :: (Board Square, [Score])
     -> [WordPut]
     -> Either String (Board Square, [Score])
  go (b,ss) pws = foldl f (Right (b,ss)) pws where
    f acc wp = do
      (b, scores) <- acc
      (b',score)  <- wordPut validate b wp dict
      return (b',scores++[score])

  -- create the wordput objects that will get put on the board
  wordPuts :: Either String [WordPut]
  wordPuts = sequence $ (\(s,o,p) -> toWordPut s o p) <$> words

  -- create a single wordput
  toWordPut :: String -> Orientation -> Point -> Either String WordPut
  toWordPut w o p = WordPut <$> tiles where
    tiles :: Either String [TilePut]
    tiles = sequence $ zipWith f w (p : allAfterByOrientationP o p) where
      f :: Char -> Point -> Either String TilePut
      f c p = LetterTilePut <$> tileFromCharEither (toUpper c) <*> pure p


