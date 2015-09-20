{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Scrabble.Board where

import Data.Char (toUpper)
import Data.Either (isRight)
import Data.List (intercalate)
import Data.Maybe (Maybe)
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude hiding (Word)
import Scrabble.Bag
import Scrabble.Types

data Bonus  = W3 | W2 | L3 | L2 | Star | NoBonus deriving (Eq,Ord)

data Square = Square {
  tile  :: Maybe Tile,
  bonus :: Bonus,
  pos   :: Position
} deriving (Eq,Ord)

instance Show Square where
  show = showSquare True

type ListBoard  = [[Square]]

class Vec m where
  before :: Int -> m a -> m a
  after  :: Int -> m a -> m a

class Matrix m where
  elemAt  :: Pos p => (m (m a)) -> p -> a
  row     :: m (m a) -> Int -> m a
  col     :: m (m a) -> Int -> m a
  rows    :: m (m a) -> m (m a)
  cols    :: m (m a) -> m (m a)
  above   :: Pos p => m (m a) -> p -> (m a)
  below   :: Pos p => m (m a) -> p -> (m a)
  leftOf  :: Pos p => m (m a) -> p -> (m a)
  rightOf :: Pos p => m (m a) -> p -> (m a)

class Matrix b => Board b where
  putTile      :: Pos p => b (b Square) -> p -> Tile -> b (b Square)
  putWord      :: b (b Square) -> PutWord -> (b (b Square), Score)
  displayBoard :: b (b Square) -> String
  -- returns Nothing if no tile on the square
  getWordAt    :: Pos p => b (b Square) -> p -> Orientation -> Maybe [Square]

getWordsAt :: (Pos p, Board b) => b (b Square) -> p -> (Maybe [Square], Maybe [Square])
getWordsAt b p = (getWordAt b p Horizontal, getWordAt b p Vertical)

instance Vec [] where
  before = take
  after  = drop . (+1)

instance Matrix [] where
  elemAt  m p = m !! y !! x where (x,y) = coors p
  row     m y = m !! y
  col     m x = fmap (!!x) m
  rows    m   = m
  cols    m   = fmap (col m) [0..14]
  above   m p = before (y p) $ col m (x p)
  below   m p = after  (y p) $ col m (x p)
  leftOf  m p = before (x p) $ row m (y p)
  rightOf m p = after  (x p) $ row m (y p)

instance Board [] where
  putTile b p t              = putTileOnListBoard b p t
  putWord b (PutWord ts o p) = putWordOnListBoard p ts o b
  displayBoard               = showListBoard True
  getWordAt                  = listBoardGetWordAt

instance Show Bonus where
  show W3 = "3W"
  show W2 = "2W"
  show L3 = "3L"
  show L2 = "2L"
  show Star    = " *"
  show NoBonus = "  "

showSquare :: Bool -> Square -> String
showSquare printBonus (Square mt b p) =
  maybe (if printBonus then show b else "  ") (\t -> [' ', letter t]) mt

debugSquare :: Square -> String
debugSquare (Square mt b p) = concat ["Square {tile:", show mt, ", bonus:", show b, ", pos:", show p]

debugSquareList :: [Square] -> String
debugSquareList ss = show $ debugSquare <$> ss

showListBoard :: Bool -> ListBoard -> String
showListBoard printBonuses board = top ++ showRows ++ bottom where
  showRows      = intercalate "\n" (fmap showRow board) ++ "\n"
  showRow     r = "|" ++ concat (fmap showSquare' r)
  showSquare' s = showSquare printBonuses s ++ "|"
  top           = line '_'
  bottom        = line '-'
  line        c = replicate 46 c ++ "\n"

printListBoard :: Bool -> ListBoard -> IO ()
printListBoard b = putStrLn . showListBoard b

{- create a new, empty Scrabble board -}
newBoard :: ListBoard
newBoard = fmap (fmap f) boardBonuses where f (pos,b) = Square Nothing b pos

{- a simple representation of an empty Scrabble board -}
boardBonuses :: [[(Position, Bonus)]]
boardBonuses = indexify [
  [W3,  o,  o, L2,  o,  o,  o, W3,  o,  o,  o, L2,  o,  o, W3],
  [ o, W2,  o,  o,  o, L3,  o,  o,  o, L3,  o,  o,  o, W2,  o],
  [ o,  o, W2,  o,  o,  o, L2,  o, L2,  o,  o,  o, W2,  o,  o],
  [L2,  o,  o, W2,  o,  o,  o, L2,  o,  o,  o, W2,  o,  o, L2],
  [ o,  o,  o,  o, W2,  o,  o,  o,  o,  o, W2,  o,  o,  o,  o],
  [ o, L3,  o,  o,  o, L3,  o,  o,  o, L3,  o,  o,  o, L3,  o],
  [ o,  o, L2,  o,  o,  o, L2,  o, L2,  o,  o,  o, L2,  o,  o],
  [W3,  o,  o, L2,  o,  o,  o,  s,  o,  o,  o, L2,  o,  o, W3],
  [ o,  o, L2,  o,  o,  o, L2,  o, L2,  o,  o,  o, L2,  o,  o],
  [ o, L3,  o,  o,  o, L3,  o,  o,  o, L3,  o,  o,  o, L3,  o],
  [ o,  o,  o,  o, W2,  o,  o,  o,  o,  o, W2,  o,  o,  o,  o],
  [L2,  o,  o, W2,  o,  o,  o, L2,  o,  o,  o, W2,  o,  o, L2],
  [ o,  o, W2,  o,  o,  o, L2,  o, L2,  o,  o,  o, W2,  o,  o],
  [ o, W2,  o,  o,  o, L3,  o,  o,  o, L3,  o,  o,  o, W2,  o],
  [W3,  o,  o, L2,  o,  o,  o, W3,  o,  o,  o, L2,  o,  o, W3]]
 where
   o = NoBonus
   s = Star
   indexify :: [[a]] -> [[(Position, a)]]
   indexify as  = fmap f (zip [0..] as) where
     f (y,l)   = fmap g (zip [0..] l ) where
       g (x,a) = (Position x y, a)

{- all the tiles 'before' a position in a matrix, vertically or horizontally -}
beforeByOrientation :: (Pos p, Matrix m) => Orientation -> m (m a) -> p -> m a
beforeByOrientation = catOrientation leftOf above
afterByOrientation :: (Pos p, Matrix m) => Orientation -> m (m a) -> p -> m a
{- all the tiles 'after' a position in a matrix, vertically or horizontally -}
afterByOrientation = catOrientation rightOf below

taken :: Square -> Bool
taken = Maybe.isJust . tile

{- a tile on a square could start two words (horizontal and vertical)
   or just one of those, or neither. return which ones it starts, if any
-}
getWordsStartedBySquare :: (Foldable b, Board b) => Square -> b (b Square) -> [[Square]]
getWordsStartedBySquare (Square (Just _) _ p) b = Maybe.catMaybes [v,h] where
  isStartOfWord :: Orientation -> Maybe [Square]
  isStartOfWord o = if (null $ beforeByOrientation o b p) then getWordAt b p o else Nothing
  v = isStartOfWord Vertical
  h = isStartOfWord Horizontal
getWordsStartedBySquare _ _ = []

{- get the word at the giving position, by orientation, if one exists -}
listBoardGetWordAt :: Pos p => ListBoard -> p -> Orientation -> Maybe [Square]
listBoardGetWordAt b p o = tile here >>= f where
  here       = elemAt b p
  beforeHere = reverse . taker . reverse $ beforeByOrientation o b p
  afterHere  = taker $ afterByOrientation o b p
  taker      = takeWhile taken
  word       = beforeHere ++ [here] ++ afterHere
  f _        = if length word > 0 then Just word else Nothing

{- place a single tile, without worrying about scoring -}
putTileOnListBoard :: Pos p => [[Square]] -> p -> Tile -> [[Square]]
putTileOnListBoard b p t = mapNth newRow (y p) b where
  newRow = mapNth (\(Square _ b p) -> Square (Just t) b p) (x p)
  mapNth :: (a -> a) -> Int -> [a] -> [a]
  mapNth f i as = xs ++ [f $ head ys] ++ drop 1 ys where (xs,ys) = splitAt i as

{- lay tiles down on the board. calculate the score of the move -}
putWordOnListBoard :: Pos p => p -> PutTiles -> Orientation -> ListBoard -> (ListBoard, Score)
putWordOnListBoard p w o b = either error (const (nextBoard, turnScore)) runChecks where
  runChecks = check p w squaresAndTiles
  turnScore = calculateScore squaresPlayedThisTurn nextBoard

  squaresInMainWord   :: [Square]
  squaresInMainWord   = take (length $ tiles w) (elemAt b p : afterByOrientation o b p)
  squaresAndTiles     :: [(Square,Maybe PutTile)]
  squaresAndTiles     = zip squaresInMainWord (tiles w)
  squaresPlayedThisTurn :: [Square]
  squaresPlayedThisTurn = f <$> filter (Maybe.isJust . snd) squaresAndTiles where
    f (Square _ b p, (Just (PutLetterTile t))) = Square (Just t) b p
    f (Square _ b p, (Just (PutBlankTile l)))  = Square (Just $ mkTile l) b p
  nextBoard :: ListBoard
  nextBoard = foldl f b squaresPlayedThisTurn where
    f acc (Square t _ p) = putTile acc p $ Maybe.fromJust t

{- checks if everything in a move is good -}
check :: Pos p =>
  p ->        -- the position of the first letter placed by the player
  PutTiles -> -- the tiles placed (or used) by the plater
  [(Square,Maybe PutTile)] -> -- the squares on the current board, paired with what the user played
  Either String ()
check p w squaresAndTiles = tooLong x >> tooLong y >> checkPuts >> return () where
  {- checkPuts returns true if
       * all the letters were put down on empty squares
       * a tile was placed in all the empty tiles in the word
   -}
  checkPuts :: Either String ()
  checkPuts = traverse (uncurry checkPut) squaresAndTiles >> return ()
  tooLong f  = if f p + (length . tiles) w > 15 then Left tooLongErr else Right ()
  tooLongErr = "word too long: " ++ show (w, (x p, y p))

  checkPut :: Square -> Maybe PutTile -> Either String ()
  checkPut (Square (Just t) _ p) (Just _) = Left $ "square taken: " ++ show t ++ ", " ++ show p
  checkPut (Square Nothing  _ p) Nothing  = Left $ "empty square at: " ++ show (x p, y p)
  checkPut _ _ = Right ()

{- Calculate the score for ALL words in a turn -}
calculateScore ::
  [Square]  -> -- all the squares a player placed tiles in this turn
  ListBoard -> -- the board (with those tiles on it)
  Score
calculateScore squaresPlayedThisTurn nextBoard = turnScore where
  wordsPlayedThisTurn :: [[Square]]
  wordsPlayedThisTurn = concat $ f <$> squaresPlayedThisTurn where
    f s = getWordsStartedBySquare s nextBoard
  squaresSet :: Set Square
  squaresSet = Set.fromList squaresPlayedThisTurn
  turnScore :: Score
  turnScore = foldl f 0 wordsPlayedThisTurn where
    f acc w = scoreWord w squaresSet + acc

{- calculate the score for a single word -}
scoreWord ::
  [Square]   -> -- one of the words played this turn
  Set Square -> -- the set of squares played in this turn
  Score
scoreWord word playedSquares = base * wordMultiplier where
  -- score all the letters
  base = foldl (\s l -> scoreLetter l playedSquares + s) 0 word

  {- the score for a single letter (including its multiplier) -}
  scoreLetter :: Square -> Set Square -> Score
  scoreLetter s@(Square (Just t) bonus _) playedSquares =
    if Set.member s playedSquares then letterBonus t bonus else score t

  {- determine the word multipliers for this word (based on using any 2W or 3W tiles -}
  wordMultiplier = foldl f 1 $ filter (\s -> Set.member s playedSquares) word where
    f acc (Square _ W3 _) = acc * 3
    f acc (Square _ W2 _) = acc * 2
    f acc _               = acc * 1

  {- determine the multipliers for a single letter -}
  letterBonus :: Tile -> Bonus -> Score
  letterBonus t L3 = 3 * score t
  letterBonus t L2 = 2 * score t
  letterBonus t _  = score t

