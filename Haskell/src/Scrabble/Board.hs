module Scrabble.Board where

import Data.Char (toUpper)
import Data.List (intercalate)
import Data.Maybe (fromMaybe,isJust)
import Prelude hiding (Word)
import Scrabble.Types

data Bonus  = W3 | W2 | L3 | L2 | Star | NoBonus deriving Eq

data Square = Square {
  tile  :: Maybe Tile,
  bonus :: Bonus,
  pos   :: Position
} deriving Eq

type Row    = [Square]
type Col    = [Square]
data Board  = Board [Row] deriving Eq

instance Show Bonus where
  show = showBonus

showBonus :: Bonus -> String
showBonus W3 = "3W"
showBonus W2 = "2W"
showBonus L3 = "3L"
showBonus L2 = "2L"
showBonus Star    = " *"
showBonus NoBonus = "  "

letterScore :: Tile -> Bonus -> Score
letterScore t L3 = 3 * (score t)
letterScore t L2 = 2 * (score t)
letterScore t _  = score t

wordBonus :: Bonus -> Score -> Score
wordBonus W3 s = s * 3
wordBonus W2 s = s * 2
wordBonus _  s = s

--instance Show Square where
--  show = showSquare True

isTaken :: Square -> Bool
isTaken (Square (Just _) _ _) = True
isTaken _ = False

showSquare :: Bool -> Square -> String
showSquare printBonus (Square mt b p) =
  maybe (if printBonus then show b else "  ") (\t -> [' ', letter t]) mt

debugSquare :: Square -> String
debugSquare (Square mt b p) = concat ["Square {tile:", show mt, ", bonus:", show b, ", pos:", show p]

instance Show Board where
  show = showBoard True

showBoard :: Bool -> Board -> String
showBoard printBonuses (Board board) = top ++ showRows ++ bottom where
  showRows      = intercalate "\n" (fmap showRow board) ++ "\n"
  showRow     r = "|" ++ concat (fmap showSquare' r)
  showSquare' s = showSquare printBonuses s ++ "|"
  top           = line '_'
  bottom        = line '-'
  line        c = replicate 46 c ++ "\n"

printBoard :: Bool -> Board -> IO ()
printBoard b = putStrLn . showBoard b

newBoard :: Board
newBoard = Board $ fmap (fmap f) boardBonuses where
  f (b,pos) = Square Nothing b pos

boardBonuses :: [[(Bonus,Position)]]
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
   indexify :: [[a]] -> [[(a, Position)]]
   indexify as = fmap f (zip as [0..]) where
     f (l,y)   = fmap g (zip  l [0..]) where
       g (a,x) = (a, Position x y)

{-
placeLetter :: (Int, Int) -> Tile -> Board -> Board
placeLetter (x,_) _ _ | x > 14 = error $ "x index out of bounds: " ++ show x
placeLetter (_,y) _ _ | y > 14 = error $ "y index out of bounds: " ++ show y
placeLetter (x,y) newLetter b = replaceRow y newRow b where
  newRow = mapNth x (\s -> placeTileInSquare s newLetter) (getRow b y)
-}

placeWord :: (Int, Int) -> Orientation -> Pattern -> Board -> (Board, Score)
placeWord (x,_) Horizontal w _ | (x + length (tiles w)) > 15 = error $ "word too long: " ++ show (x, w)
placeWord (_,y) Vertical   w _ | (y + length (tiles w)) > 15 = error $ "word too long: " ++ show (y, w)
placeWord (x,y) Horizontal w b = (replaceRow y newRow b, score) where
  (newRow,score) = placeWordInRow x (getRow b y) w
placeWord (x,y) Vertical   w b = (replaceCol x newRow b, score) where
  (newRow,score) = placeWordInCol y (getCol b x) w

getRow :: Board -> Int -> Row
getRow (Board rows) n = rows !! n

getCol :: Board -> Int -> Col
getCol (Board rows) n = fmap (!! n) rows

placeWordInRow :: Int -> Row -> Pattern -> (Row, Score)
placeWordInRow = placeWordInList
placeWordInCol :: Int -> Col -> Pattern -> (Col, Score)
placeWordInCol = placeWordInList

placeWordInList :: Int -> [Square] -> Pattern -> (Row, Score)
placeWordInList pos row pat = (newRow, wordScore) where
  (xs,ys)    = splitAt pos row
  ysStart    = take (length $ tiles pat) ys
  ysEnd      = drop (length $ tiles pat) ys
  newSquares = fmap (uncurry placeTileInSquare) (zip ysStart $ tiles pat)
  newRow     = xs ++ newSquares ++ ysEnd
  wordScore  = getScore $ zip (tiles pat) newSquares

getScore ::  [(Maybe Tile, Square)] -> Int
getScore placements = foldl (flip wordBonus) lScore wordBonuses where
  lScore = sum . fmap lScore1 $ placements where
    lScore1 (Nothing, (Square (Just t) b _)) = score t
    lScore1 (Just t,  (Square _ b _))        = score t
  wordBonuses :: [Bonus]
  wordBonuses = fmap (\(_,s) -> bonus s) (filter (\(m,s) -> isJust m) placements)

placeTileInSquare :: Square -> Maybe Tile -> Square
-- if it is taken, and we are trying to put a tile there, bad.
placeTileInSquare s (Just _) | isTaken s = error $ "Square unavailable: " ++ debugSquare s
-- if it isn't taken, and we want to put a tile in, good.
placeTileInSquare (Square Nothing b p) (Just t) = Square (Just t) b p
-- if aren't trying to put anything in there, thats ok..
placeTileInSquare s Nothing  = s

replaceRow :: Int -> Row -> Board -> Board
replaceRow n newRow (Board b) = Board $ replaceNth n newRow b

replaceCol :: Int -> Col -> Board -> Board
replaceCol n newCol (Board rows) =
  Board $ fmap replaceLetterInRow (zip newCol rows) where
    replaceLetterInRow (l,r) = replaceNth n l r

replaceNth :: Int -> a -> [a] -> [a]
replaceNth n newVal = mapNth n (const newVal)

mapNth :: Int -> (a -> a) -> [a] -> [a]
mapNth n f as = xs ++ [f y] ++ ys where (xs,y:ys) = splitAt n as

-- Might want to use this later...
replaceNthIfAvailable :: Show a => Int -> a -> [Maybe a] -> [Maybe a]
replaceNthIfAvailable n newVal row = case row !! n of
  Just n' -> error $ show ("position taken", n, newVal, row)
  Nothing -> replaceNth n (Just newVal) row
