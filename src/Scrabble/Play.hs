{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Scrabble.Play where

import Data.Char (toUpper)
import Data.List (any, delete, foldl', groupBy, intersperse, partition, sort)
import Data.Maybe (Maybe)
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude hiding (Word)
import Scrabble.Bag
import Scrabble.Board
import Scrabble.Game
import Scrabble.Dictionary
import Scrabble.Matrix
import Scrabble.Position
import Scrabble.Search (containsAll)

data Move b = Move {
  pointsScored   :: Points
 ,remaining      :: Rack
 ,boardAfterMove :: b Square }

{-
 represents a single tile being put on the board (without location)
   * PutLetterTile means the tile has a letter on it
   * PutBlankTile comes with the Letter that the player intends use.
-}
data PutTile =
   PutLetterTile Tile   Position
 | PutBlankTile  Letter Position
  deriving Eq

-- TODO: when are these shown? show the position be shown too?
instance Show PutTile where
  show (PutLetterTile t _) = show $ letter t
  show (PutBlankTile  l _) = show l

instance HasLetter PutTile where
  letter (PutLetterTile t _) = letter t
  letter (PutBlankTile  l _) = l

instance HasPosition PutTile where
  pos (PutLetterTile _ p) = p
  pos (PutBlankTile  _ p) = p

asTile (PutLetterTile t _) = t
asTile (PutBlankTile  l _) = fromLetter l

{- A complete representation of placing a word on the board. -}
data PutWord = PutWord { tiles :: [PutTile] } deriving Show

{- calculate the score for a single word -}
scoreWord ::
  [Square]   -> -- one of the words played this turn
                -- (and the one we are getting the score for)
  Set Square -> -- the set of squares played in this turn
  Score
scoreWord word playedSquares =
  base * wordMultiplier * centerMultiplier where

  -- score all the letters
  base = foldl (\s l -> scoreLetter l playedSquares + s) 0 word

  {- the score for a single letter (including its multiplier) -}
  scoreLetter :: Square -> Set Square -> Score
  scoreLetter s@(Square (Just t) bonus _) playedSquares =
    if Set.member s playedSquares then letterBonus t bonus else score t

  {- determine the word multipliers for this word
    (based on using any 2W or 3W tiles -}
  wordMultiplier = foldl' f 1 $ filter g word where
    f acc (Square _ W3 _) = acc * 3
    f acc (Square _ W2 _) = acc * 2
    f acc _               = acc * 1
    g s = Set.member s playedSquares

  {- determine the multipliers for a single letter -}
  letterBonus :: Tile -> Bonus -> Score
  letterBonus t L3 = 3 * score t
  letterBonus t L2 = 2 * score t
  letterBonus t _  = score t

  {- if the center square was played, score*2 -}
  centerMultiplier = if centerSquarePlayed then 2 else 1
  centerSquarePlayed = or $ f <$> Set.toList playedSquares where
    f s = pos s == centerPosition

{- lay tiles on the board.
   validate all the things.
   calculate the score of the move. -}
putWord :: (Foldable b, Board b, Vec (Row b))  =>
           b Square ->
           PutWord  ->
           Dict     -> -- the dictionary
           Either String (b Square, Score)
putWord b pw dict = do
  squares <- squaresPlayedThisTurn
  let b' = nextBoard $ zip squares (tiles pw)
  validateMove (zipSquaresAndTiles squares) b b' dict
  return (b', calculateTurnScore squares b') where

  squaresPlayedThisTurn :: Either String [Square]
  squaresPlayedThisTurn = traverse f (pos <$> tiles pw) where
    f :: Position -> Either String Square
    f p = maybe (Left $ "out of bounds: " ++ show p) Right $ elemAt b p

  zipSquaresAndTiles :: [Square] -> [(Square,PutTile,Position)]
  zipSquaresAndTiles sqrs = zipWith f sqrs (tiles pw) where
    f s t = (s, t, pos t)

  --TODO: the compiler won't let me put this type sig here.
  --      even with ScopedTypeVariables.
  --nextBoard :: [(Square,PutTile)] -> b Square
  nextBoard sqrs = foldl f b sqrs where
    f acc ((Square _ _ p), pt) = putTile acc p (asTile pt)

{- Calculate the score for ALL words in a turn -}
calculateTurnScore :: (Foldable b, Board b, Vec (Row b)) =>
  [Square] -> -- all the squares a player placed tiles in this turn
  b Square -> -- the board (with those tiles on it)
  Score
calculateTurnScore sqrs nextBoard = totalScore where
  totalScore = foldl f 0 s + bingoBonus
  s = Set.toList $ wordsPlayedInTurn sqrs nextBoard
  f acc w = scoreWord w (Set.fromList sqrs) + acc
  bingoBonus = if length sqrs == 7 then 50 else 0

wordsPlayedInTurn :: (Foldable b, Board b, Vec (Row b)) =>
  [Square] -> -- all the squares a player placed tiles in this turn
  b Square -> -- the board (with those tiles on it)
  Set [Square]
wordsPlayedInTurn squaresPlayedThisTurn nextBoard =
  Set.fromList . concat $ f <$> squaresPlayedThisTurn where
    f s = getWordsTouchingSquare s nextBoard

-- helper data for packing information about a
-- square played on a particular turn.
data SquareLegality = SquareLegality {
  square     :: Square
 ,available  :: Bool -- is the square not taken
 ,connected  :: Bool -- if touching another tile
 ,centerPlay :: Bool -- if square is the center square
} deriving Show

{- checks if everything in a move is good
  1) At least one tile must be tentative
  2) All tentative tiles must lie on one line
  3) On the first turn, one tile must be located on square START_POSITION
  4) Linear word must be unbroken (including locked tiles)
  5) On every other turn, at least one crossword must be formed
  6) All words formed must be inside the dictionary
-}
validateMove ::
  (Vec (Row b), Foldable b, Board b, Pos p, Show p) =>
  [(Square,PutTile,p)] -> -- all the letters put down this turn
  b Square -> -- old board
  b Square -> -- new board
  Dict     -> -- the dictionary
  Either String ()
validateMove move b b' dict = go where
  go =
    if null move
      then Left "You must place at least one tile!"
    else if emptyB && not centerSquarePlayed
      then Left "Must use center square!"
    else if emptyB && length move <= 1
      then Left "First move must have more than one letter!"
    else if not allConnected
      then Left "Unconnected letters!"
    else if not $ null takenSquares
      then Left squaresTakenError
    else if not $ null badWords
      then Left $ "Bad words: " ++ show badWords
    else Right ()

  -- so we don't have to calculate it twice
  emptyB = emptyBoard b

  legals :: [SquareLegality]
  legals = f <$> squaresPlayedInMove where
    f s = SquareLegality s
      (emptySquare s)
      (or $ taken <$> neighbors b' (pos s))
      (pos s == centerPosition)

  centerSquarePlayed = or $ centerPlay <$> legals

  allConnected :: Bool
  allConnected = and $ connected <$> legals

  squaresPlayedInMove = (\(s,_,_) -> s) <$> move

  takenSquares :: [Square]
  takenSquares = square <$> filter (not . available) legals

  words :: [Word]
  words = toWord <$> (Set.toList $ wordsPlayedInTurn squaresPlayedInMove b')

  (_, badWords) = partition (dictContainsWord dict) words

  squaresTakenError = "Error, squares taken: " ++
    show (intersperse "," $ debugSquare <$> takenSquares)

place :: (Foldable b, Board b, Vec (Row b)) =>
  Game b      ->
  String      ->
  Orientation ->
  Position    ->
  [Char]      ->
  Either String (Game b)

place g w o p blanks = do
  pw <- makePutWord (fmap toUpper w) o p blanks
  applyMove g <$> createMove
                    (gameBoard g)
                    (playerRack $ currentPlayer g)
                    pw
                    (gameDict g)

-- a lot of error handling isn't happening here
-- this code is pretty bad
makePutWord :: String      ->
               Orientation ->
               Position    ->
               [Char]      ->
               Either String PutWord
makePutWord w o p blanks = PutWord <$> putTils where

  coords :: [(Int,Int)]
  coords = reverse . fst $ foldl f ([],coors p) w where
    f (acc,p) c = (p:acc, catOrientation rightOfP belowP o p)

  putTils :: Either String [PutTile]
  putTils = Maybe.catMaybes <$> (sequence $ zipWith f w (zip coords [0..])) where
    f :: Char -> ((Int,Int),Int) -> Either String (Maybe PutTile)
    f '@' _     = Right Nothing
    f '_' (p,i) = plt (blanks !! i) (pos p)
    f  c  (p,i) = plt c (pos p)
    plt :: Char -> Position -> Either String (Maybe PutTile)
    plt c p     = Just <$> (PutLetterTile <$> maybe (err c) Right (tileFromChar c) <*> pure p)
    err c       = Left $ "invalid character: " ++ [c]

applyPutWord :: (Foldable b, Board b, Vec (Row b)) => Game b ->
                                                      PutWord ->
                                                      Either String (Game b)
applyPutWord g@(Game (p:_) board bag dict) pw =
  applyMove g <$> createMove board (playerRack p) pw dict

applyMove :: Board b => Game b -> Move b -> Game b
applyMove g@(Game (p:ps) _ bag d) (Move pts rack newBoard) =
  Game (ps++[p']) newBoard bag' d where
    (t',bag') = fillRack rack bag
    newScore  = playerScore p + pts
    p' = p {playerRack = t', playerScore = newScore}

createMove :: (Foldable b, Board b, Vec (Row b)) =>
               b Square ->
               Rack     ->
               PutWord  ->
               Dict     ->
               Either String (Move b)
createMove b rack pw dict =
  if valid then go else Left errMsg where
    errMsg        = "error: rack missing input letters"
    rackLetters   = fmap letter rack
    valid         = containsAll (toString putLetters) (toString rackLetters)
    putLetters    = letter <$> tiles pw
    rackRemainder = fmap fromLetter $
      foldl' (flip delete) rackLetters putLetters
    go = do (newBoard, score) <- putWord b pw dict
            return $ Move score rackRemainder newBoard
