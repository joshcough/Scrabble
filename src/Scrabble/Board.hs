module Scrabble.Board where

import Data.Maybe (fromMaybe)
import Data.List (intercalate)
import Prelude hiding (Word)
import Scrabble.Types

data Bonus  = W3 | W2 | L3 | L2 | NoBonus
type Row    = [Maybe Letter]
type Col    = [Maybe Letter]
data Board  = Board [Row]
data Orientation = Horizontal | Vertical

instance Show Board where
  show (Board board) = line ++ showRows ++ line where
    showRows     = intercalate "\n" (fmap showRow board) ++ "\n"
    showRow  r   = "|" ++ concat (fmap showLetter r)
    showLetter l = fromMaybe ' ' l : "|"
    line         = "-------------------------------\n"

newBoard :: Board
newBoard = Board $ surround middleRow normalRow where
  normalRow = replicate 15 Nothing
  middleRow = surround (Just '*') Nothing
  surround center other = replicate 7 other ++ [center] ++ replicate 7 other

placeLetter :: (Int, Int) -> Letter -> Board -> Board
placeLetter (x,_) _ _ | x > 14 = error $ "x index out of bounds: " ++ show x
placeLetter (_,y) _ _ | y > 14 = error $ "y index out of bounds: " ++ show y
placeLetter (x,y) newLetter b = replaceRow y newRow b where
  newRow = replaceNth x (Just newLetter) (getRow b y)

placeWord :: (Int, Int) -> Orientation -> Word -> Board -> Board
placeWord (x,_) Horizontal w _ | (x + length w) > 15 = error $ "word too long: " ++ show (x, w)
placeWord (_,y) Vertical   w _ | (y + length w) > 15 = error $ "word too long: " ++ show (y, w)
placeWord (x,y) Horizontal w b = replaceRow y (placeWordInRow x (getRow b y) w) b
placeWord (x,y) Vertical   w b = replaceCol x (placeWordInCol y (getCol b x) w) b

getRow :: Board -> Int -> Row
getRow (Board rows) n = rows !! n

getCol :: Board -> Int -> Col
getCol (Board rows) n = fmap (!! n) rows

placeWordInRow = placeWordInList
placeWordInCol = placeWordInList

-- This is where we should check if the spot is taken...
placeWordInList :: Int -> [Maybe Letter] -> Word -> Row
placeWordInList pos row word = xs ++ fmap Just word ++ ys' where
  (xs,ys) = splitAt pos row
  ys' = drop (length word) ys

replaceRow :: Int -> Row -> Board -> Board
replaceRow n newRow (Board b) = Board $ replaceNth n newRow b

replaceCol :: Int -> Col -> Board -> Board
replaceCol n newCol (Board rows) =
  Board $ fmap replaceLetterInRow (zip newCol rows) where
    replaceLetterInRow (l,r) = replaceNth n l r

replaceNth :: Int -> a -> [a] -> [a]
replaceNth n newVal as = xs ++ [newVal] ++ ys where
  (xs,_:ys) = splitAt n as

-- Might want to use this later...
replaceNthIfAvailable :: Show a => Int -> a -> [Maybe a] -> [Maybe a]
replaceNthIfAvailable n newVal row = case row !! n of
  Just n' -> error $ show ("position taken", n, newVal, row)
  Nothing -> replaceNth n (Just newVal) row
