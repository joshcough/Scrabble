module Scrabble (
  module Scrabble.Bag
 ,module Scrabble.Board
 ,module Scrabble.Commands.AST
 ,module Scrabble.Commands.SExpr
 ,module Scrabble.Search
 ,module Scrabble.Types
) where

import Data.Char (toUpper)
import Data.List (delete,groupBy)
import Data.Maybe (catMaybes)
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
  gameBoard   :: Board,
  gameBag     :: Bag,
  gameDict    :: Dict } deriving (Eq, Show)

nextPlayer :: Game -> Player
nextPlayer = head . gamePlayers

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

start :: [(Name, PlayerType)] -> IO ()
start players = newGame players >>= gameLoop

gameLoop :: Game -> IO ()
gameLoop g | isGameOver g = putStrLn ("Game Over!\n" ++ show g)
gameLoop g = singleTurn g >>= gameLoop

singleTurn :: Game -> IO Game
singleTurn g =
  if isHuman (nextPlayer g) then humanTurn g else return (aiTurn g)

humanTurn :: Game -> IO Game
humanTurn g = do
  printBoard True (gameBoard g)
  putStrLn $ "Turn for: " ++ show (nextPlayer g)
  putStrLn "Enter command (or type help)"
  command <- getLine
  interpCommandString g command

interpCommandString :: Game -> String -> IO Game
interpCommandString g command =
  either
    (\err -> putStrLn err >> humanTurn g)
    (\res -> interpCommandRes g res)
    (fromString command >>= interpretExp (gameBoard g) (playerTray (nextPlayer g)) (gameDict g))

interpCommandRes :: Game -> CommandResult -> IO Game
interpCommandRes g@(Game (p:ps) bd bag d) (MoveResult (Move points remaining updatedBoard)) =
  return $ Game (ps++[updatedPlayer]) updatedBoard updatedBag d where
    updatedPlayer = Player (playerType p) (playerName p) updatedTray (playerScore p + points)
    updatedTray   = take (7 - length remaining) bag ++ remaining
    updatedBag    = drop (7 - length remaining) bag
interpCommandRes g (QueryResult words) = putStrLn (show words) >> singleTurn g
interpCommandRes g ShowHelp            = putStrLn "help unimplemented" >> singleTurn g
interpCommandRes g NextPlayer          = return g
interpCommandRes g (PrintBoard b)      = printBoard b (gameBoard g) >> singleTurn g

aiTurn :: Game -> Game
aiTurn g = error "todo: aiTurn"

lookupWithPoints :: Search1 -> Dict -> [(Word, Points)]
lookupWithPoints search dict = fmap (\w -> (w,simpleWordPoints w)) (runSearch1 search dict)

data Move = Move { pointsScored :: Points, remaining :: Tray, boardAfterMove :: Board }
data CommandResult =
 MoveResult Move              |
 QueryResult [(Word, Points)] |
 ShowHelp                     |
 NextPlayer                   |
 PrintBoard Bool

interpretExp :: Board -> Tray -> Dict -> ScrabbleExp -> Either String CommandResult
interpretExp _ _ _ Skip = return NextPlayer
interpretExp _ _ _ Help = return ShowHelp
interpretExp _ _ _ (ShowBoard b)      = return $ PrintBoard b
interpretExp _ _ dict (Search search) = QueryResult <$> interpretSearch search dict
interpretExp b t _ (Place (Placement pattern o pos word)) =
  MoveResult <$> interpretPlacement b t pattern o pos word

interpretSearch :: SearchExp -> Dict -> Either String [(Word, Points)]
interpretSearch search dict = lookupWithPoints <$> toSearch1 search <*> pure dict

interpretPlacement ::
  Board       ->
  Tray        ->
  Pattern     ->
  Orientation ->
  Position    ->
  Maybe Word  ->
  Either String Move
interpretPlacement b tray pat o pos word =
  if   patternLetters `containsAll` trayLetters
  then return $ Move score trayRemainder newBoard
  else Left "missing letters" where
    (newBoard, score) = placeWord (x pos, y pos) o pat b
    trayLetters       = fmap letter tray
    patternLetters    = filter (\c -> c /= '@') . fmap (toUpper.letter) . catMaybes $ tiles pat
    trayRemainder     = fmap fromLetter $ foldl (flip delete) trayLetters patternLetters


