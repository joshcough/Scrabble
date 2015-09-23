{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Scrabble.Board where

import Data.Char (toUpper)
import Data.Either (isRight)
import Data.List (intercalate)
import Data.Maybe (Maybe)
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import Debug.Trace
import qualified Data.Set as Set
import Prelude hiding (Word)
import Scrabble.Bag
import Scrabble.Types

data Bonus  = W3 | W2 | L3 | L2 | Star | NoBonus deriving (Eq,Ord)

data Square = Square {
  tile      :: Maybe Tile,
  bonus     :: Bonus,
  squarePos :: Position
} deriving (Eq,Ord)

instance HasPosition Square where
  pos (Square _ _ p) = p

instance Show Square where
  show = showSquare True

type ListBoard  = [[Square]]

class Vec m where
  before :: Int -> m a -> m a
  after  :: Int -> m a -> m a

class Matrix m where
  elemAt  :: Pos p => (m (m a)) -> p -> Maybe a
  row     :: m (m a) -> Int -> Maybe (m a)
  col     :: m (m a) -> Int -> Maybe (m a)
  rows    :: m (m a) -> m (m a)
  cols    :: m (m a) -> m (m a)
  above   :: Pos p => m (m a) -> p -> m a
  below   :: Pos p => m (m a) -> p -> m a
  leftOf  :: Pos p => m (m a) -> p -> m a
  rightOf :: Pos p => m (m a) -> p -> m a

class Matrix b => Board b where
  putTile      :: Pos p => b (b Square) -> p -> Tile -> b (b Square)
  putWord      :: b (b Square) -> PutWord -> Either String (b (b Square), Score)
  displayBoard :: b (b Square) -> String
  -- returns Nothing if no tile on the square
  getWordAt    :: Pos p => b (b Square) -> p -> Orientation -> Maybe [Square]

getWordsAt :: (Pos p, Board b) => b (b Square) -> p -> (Maybe [Square], Maybe [Square])
getWordsAt b p = (getWordAt b p Horizontal, getWordAt b p Vertical)

instance Vec [] where
  before = take
  after  = drop . (+1)

instance Matrix [] where
  elemAt  m p | x >= 0 && y >= 0 = Just (m !! y !! x) where (x,y) = coors p
  elemAt  _ _ = Nothing
  row     m y | y >= 0 = Just $ m !! y
  row     _ _ = Nothing
  col     m x | x >= 0 = Just $ fmap (!!x) m
  col     _ _ = Nothing
  rows    m   = m
  cols    m   = Maybe.catMaybes $ fmap (col m) [0..14]
  above   m p = Maybe.fromMaybe [] $ before (y p) <$> col m (x p)
  below   m p = Maybe.fromMaybe [] $ after  (y p) <$> col m (x p)
  leftOf  m p = Maybe.fromMaybe [] $ before (x p) <$> row m (y p)
  rightOf m p = Maybe.fromMaybe [] $ after  (x p) <$> row m (y p)

instance Board [] where
  putTile b p t  = putTileOnListBoard b p t
  putWord b pw   = putWordOnListBoard b pw
  displayBoard   = showListBoard True
  getWordAt      = listBoardGetWordAt

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

getWordsTouchingSquare :: (Foldable b, Board b) => Square -> b (b Square) -> [[Square]]
getWordsTouchingSquare s b = Maybe.catMaybes [mh,mv] where (mh,mv) = getWordsAt b (pos s)

{- get the word at the giving position, by orientation, if one exists -}
listBoardGetWordAt :: Pos p => ListBoard -> p -> Orientation -> Maybe [Square]
listBoardGetWordAt b p o = tile <$> here >>= f where
  here       = elemAt b p
  beforeHere = reverse . taker . reverse $ beforeByOrientation o b p
  afterHere  = taker $ afterByOrientation o b p
  taker      = takeWhile taken
  word       = beforeHere ++ maybe [] (:[]) here ++ afterHere
  f _        = if length word > 1 then Just word else Nothing

{- place a single tile, without worrying about scoring -}
putTileOnListBoard :: Pos p => [[Square]] -> p -> Tile -> [[Square]]
putTileOnListBoard b p t = mapNth newRow (y p) b where
  newRow = mapNth (\(Square _ b p) -> Square (Just t) b p) (x p)
  mapNth :: (a -> a) -> Int -> [a] -> [a]
  mapNth f i as = xs ++ [f $ head ys] ++ drop 1 ys where (xs,ys) = splitAt i as

{- lay tiles down on the board. calculate the score of the move -}
putWordOnListBoard :: ListBoard -> PutWord -> Either String (ListBoard, Score)
putWordOnListBoard b pw = do
  squares <- squaresPlayedThisTurn
  let b' = nextBoard squares
  runChecks (zipSquaresAndTiles squares) b b'
  return (b', turnScore squares b') where

  turnScore :: [Square] -> ListBoard -> Int
  turnScore sqrs brd = calculateScore sqrs brd

  squaresPlayedThisTurn :: Either String [Square]
  squaresPlayedThisTurn = traverse f (pos <$> tiles pw) where
    f :: Position -> Either String Square
    f p = maybe (Left $ "out of bounds: " ++ show p) Right $ elemAt b p

  zipSquaresAndTiles :: [Square] -> [(Square,PutTile,Position)]
  zipSquaresAndTiles sqrs = zipWith (\s t -> (s, t, pos t)) sqrs (tiles pw)

  nextBoard :: [Square] -> ListBoard
  nextBoard sqrs = foldl f b sqrs where
    f acc (Square t _ p) = putTile acc p $ Maybe.fromJust t

{- checks if everything in a move is good -}
runChecks ::
  [(Square,PutTile,Position)] -> -- all the letters put down this turn
  ListBoard -> -- old board
  ListBoard -> -- new board
  Either String ()
runChecks squaresAndTiles b b' = checkPuts >> return () where
  firstPos = pos . (\(_,_,p) -> p) $ head squaresAndTiles

  {- TODO: I think check puts should happen before this,
           when the PutTiles are created.
   -}
  {- checkPuts returns true if
       * all the letters were put down on empty squares
       * a tile was placed in all the empty tiles in the word
   -}
  checkPuts :: Either String ()
  checkPuts = traverse checkPut squaresAndTiles >> return () where
    -- make sure we haven't put something in a tile thats already taken
    checkPut :: (Square, PutTile, Position) -> Either String ()
    checkPut ((Square (Just t) _ _), _, p) =
      Left $ "square taken: " ++ show t ++ ", " ++ show p
    checkPut _ = Right ()

{- Calculate the score for ALL words in a turn -}
calculateScore ::
  [Square]  -> -- all the squares a player placed tiles in this turn
  ListBoard -> -- the board (with those tiles on it)
  Score
calculateScore squaresPlayedThisTurn nextBoard = turnScore where
  wordsPlayedThisTurn :: Set [Square]
  wordsPlayedThisTurn = Set.fromList . concat $ f <$> squaresPlayedThisTurn where
    f s = getWordsTouchingSquare s nextBoard
  squaresSet :: Set Square
  squaresSet = Set.fromList squaresPlayedThisTurn
  turnScore :: Score
  turnScore = foldl f 0 (trace ("wordsPlayedThisTurn: " ++ show wordsPlayedThisTurn) (Set.toList wordsPlayedThisTurn)) where
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

{- put some words on a brand new board -}
quickPut :: [(String, Orientation, (Int, Int))] ->
            Either String (ListBoard,[Score])
quickPut words = quickPut' words newBoard

{- put some words onto an existing board -}
quickPut' :: [(String, Orientation, (Int, Int))] ->
             ListBoard ->
             Either String (ListBoard,[Score])
quickPut' words b = go (b,[]) putWords where

  {- TODO: this is pretty awful
     I think EitherT over State could clean it up,
     but not sure if i want to do that.
  -}
  go :: (ListBoard, [Score]) -> [PutWord] -> Either String (ListBoard, [Score])
  go (b,ss) pws = foldl f (Right (b,ss)) pws where
    f acc pw = do
      (b,scores) <- acc
      (b',score) <- putWord b pw
      return (b',score:scores)

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
