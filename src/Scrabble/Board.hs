module Scrabble.Board where

import Data.Char (toUpper)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Prelude hiding (Word)
import Scrabble.Types

data Bonus  = W3 | W2 | L3 | L2 | Star | NoBonus
data Square = Square (Maybe Letter) Bonus
type Row    = [Square]
type Col    = [Square]
data Board  = Board [Row]
data Orientation = Horizontal | Vertical

instance Show Bonus where
  show = showBonus

showBonus :: Bonus -> String
showBonus W3 = "3W"
showBonus W2 = "2W"
showBonus L3 = "3L"
showBonus L2 = "2L"
showBonus Star    = " *"
showBonus NoBonus = "  "

instance Show Square where
  show = showSquare True

showSquare :: Bool -> Square -> String
showSquare printBonus (Square ml b) =
  maybe (if printBonus then show b else "  ") (\c -> [' ', toUpper c]) ml

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

printBoard :: Board -> IO ()
printBoard = putStrLn . showBoard True

printBoardClean :: Board -> IO ()
printBoardClean = putStrLn . showBoard False

newBoard :: Board
newBoard = Board $ fmap (fmap f) [
  [W3,  o,  o, L2,  o,  o,  o, W3,  o,  o,  o, L2,  o,  o, W3],
  [ o, W2,  o,  o,  o, L3,  o,  o,  o, L3,  o,  o,  o, W2,  o],
  [ o,  o, W2,  o,  o,  o, L2,  o, L2,  o,  o,  o, W2,  o,  o],
  [L2,  o,  o, W2,  o,  o,  o, L2,  o,  o,  o, W2,  o,  o,  o],
  [ o,  o,  o,  o, W2,  o,  o,  o,  o,  o, W2,  o,  o,  o,  o],
  [ o, L3,  o,  o,  o, L3,  o,  o,  o, L3,  o,  o,  o, L3,  o],
  [ o,  o, L2,  o,  o,  o, L2,  o, L2,  o,  o,  o, L2,  o,  o],
  [W3,  o,  o, L2,  o,  o,  o,  s,  o,  o,  o, L2,  o,  o, W3],
  [ o,  o, L2,  o,  o,  o, L2,  o, L2,  o,  o,  o, L2,  o,  o],
  [ o, L3,  o,  o,  o, L3,  o,  o,  o, L3,  o,  o,  o, L3,  o],
  [ o,  o,  o,  o, W2,  o,  o,  o,  o,  o, W2,  o,  o,  o,  o],
  [L2,  o,  o, W2,  o,  o,  o, L2,  o,  o,  o, W2,  o,  o,  o],
  [ o,  o, W2,  o,  o,  o, L2,  o, L2,  o,  o,  o, W2,  o,  o],
  [ o, W2,  o,  o,  o, L3,  o,  o,  o, L3,  o,  o,  o, W2,  o],
  [W3,  o,  o, L2,  o,  o,  o, W3,  o,  o,  o, L2,  o,  o, W3]]
 where
   o = NoBonus
   s = Star
   f = Square Nothing

placeLetter :: (Int, Int) -> Letter -> Board -> Board
placeLetter (x,_) _ _ | x > 14 = error $ "x index out of bounds: " ++ show x
placeLetter (_,y) _ _ | y > 14 = error $ "y index out of bounds: " ++ show y
placeLetter (x,y) newLetter b = replaceRow y newRow b where
  newRow = mapNth x (\s -> placeLetterInSquare s newLetter) (getRow b y)

placeWord :: (Int, Int) -> Orientation -> Word -> Board -> Board
placeWord (x,_) Horizontal w _ | (x + length w) > 15 = error $ "word too long: " ++ show (x, w)
placeWord (_,y) Vertical   w _ | (y + length w) > 15 = error $ "word too long: " ++ show (y, w)
placeWord (x,y) Horizontal w b = replaceRow y (placeWordInRow x (getRow b y) w) b
placeWord (x,y) Vertical   w b = replaceCol x (placeWordInCol y (getCol b x) w) b

getRow :: Board -> Int -> Row
getRow (Board rows) n = rows !! n

getCol :: Board -> Int -> Col
getCol (Board rows) n = fmap (!! n) rows

placeWordInRow :: Int -> Row -> Word -> Row
placeWordInRow = placeWordInList
placeWordInCol :: Int -> Col -> Word -> Col
placeWordInCol = placeWordInList

-- TODO: this is where we would calculate score
placeWordInList :: Int -> [Square] -> Word -> Row
placeWordInList pos row word = xs ++ newSquares ++ ysEnd where
  (xs,ys) = splitAt pos row
  ysStart = take (length word) ys
  ysEnd   = drop (length word) ys
  newSquares = fmap (uncurry placeLetterInSquare) (zip ysStart word)

-- TODO: This is where we should check if the spot is taken...
placeLetterInSquare :: Square -> Letter -> Square
placeLetterInSquare (Square ml b) l = Square (Just l) b

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
