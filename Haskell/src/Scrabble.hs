module Scrabble (
  module Scrabble.Bag
 ,module Scrabble.Board
 ,module Scrabble.Commands.AST
 ,module Scrabble.Commands.SExpr
 ,module Scrabble.Search
 ,module Scrabble.Types
) where

import Scrabble.Bag
import Scrabble.Board
import Scrabble.Commands.AST
import Scrabble.Commands.SExpr
import Scrabble.Search
import Scrabble.Types
import Prelude hiding (Word)

type Name  = String
type Score = Int
type Dict  = [Word]

data PlayerType = Human | AI deriving (Eq, Show)
data Player = Player {
  playerType  :: PlayerType,
  playerName  :: Name,
  playerTray  :: Tray,
  playerScore :: Score } deriving (Eq, Show)

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
gameLoop g = gameLoop' (cycle $ gamePlayers g) g where
  gameLoop' _ g | isGameOver g = putStrLn ("Game Over!\n" ++ show g)
  gameLoop' (p:ps) g = singleTurn p g >>= gameLoop' ps

singleTurn :: Player -> Game -> IO Game
singleTurn p | isHuman p = humanTurn p
singleTurn ai = return . aiTurn ai

humanTurn :: Player -> Game -> IO Game
humanTurn p g = do
  printBoard True (gameBoard g)
  putStrLn $ "Turn for: " ++ show p
  putStrLn "Enter command (or type help)"
  command <- getLine
  interpCommandString p g command

interpCommandString :: Player -> Game -> String -> IO Game
interpCommandString p g command =
  either
    (\err -> putStrLn err >> humanTurn p g)
    (\res -> interpCommandRes p g res)
    (fromString command >>= interpretExp (gameBoard g) (playerTray p) (gameDict g))

interpCommandRes :: Player -> Game -> CommandResult -> IO Game
interpCommandRes h g@(Game ps bd bg d) (MoveResult (Move p r b)) = return $ Game ps b bg d
interpCommandRes p g (QueryResult words) = putStrLn (show words) >> singleTurn p g
interpCommandRes p g ShowHelp            = putStrLn "help unimplemented" >> singleTurn p g
interpCommandRes h g NextPlayer          = return g
interpCommandRes p g (PrintBoard b)      = printBoard b (gameBoard g) >> singleTurn p g

aiTurn :: Player -> Game -> Game
aiTurn p g = error "todo: aiTurn"

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
interpretPlacement b t pat o pos word =
  -- just doing the simplest thing possible for now
  return $ Move 0 t (placeWord (x pos, y pos) o pat b)


