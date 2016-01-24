{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Scrabble.Play where

import Data.List (any, foldl', intersperse, partition)
import Data.Maybe (Maybe)
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Scrabble.Bag
import Scrabble.Board
import Scrabble.Matrix
import Scrabble.Search (dictContainsWord)
import Scrabble.Types

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
  show (PutLetterTile t _) = [letter t]
  show (PutBlankTile  l _) = [l]

instance HasLetter PutTile where
  letter (PutLetterTile t _) = letter t
  letter (PutBlankTile  l _) = l

instance HasPosition PutTile where
  pos (PutLetterTile _ p) = p
  pos (PutBlankTile  _ p) = p

asTile (PutLetterTile t _) = t
asTile (PutBlankTile  l _) = mkTile l

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

  words :: [[Letter]]
  words = toWord <$> (Set.toList $
    wordsPlayedInTurn squaresPlayedInMove b')

  (_, badWords) = partition (dictContainsWord dict) words

  squaresTakenError = "Error, squares taken: " ++
    show (intersperse "," $ debugSquare <$> takenSquares)

{- test putting some words onto an existing board -}
putManyWords :: (Foldable b, Board b, Vec (Row b)) =>
  [(String, Orientation, (Int, Int))] ->
  b Square ->
  Dict     ->
  Either String (b Square,[Score])
putManyWords words b dict = go (b,[]) putWords where
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
