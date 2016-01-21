{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Scrabble.ListBoard where

import Data.List (intercalate)
import Scrabble.Bag
import Scrabble.Board
import Scrabble.Matrix
import Scrabble.Position

type ListBoard = ListMatrix Square

instance Board ListMatrix where
  putTile   = putTileOnListBoard
  showBoard = showListBoard
  newBoard  = newListBoard

showListBoard :: ListBoard -> Bool -> String
showListBoard (LM board) printBonuses = top ++ showRows ++ bottom where
  showRows      = intercalate "\n" (fmap showRow board) ++ "\n"
  showRow     r = "|" ++ concat (fmap showSquare' r)
  showSquare' s = showSquare printBonuses s ++ "|"
  top           = line '_'
  bottom        = line '-'
  line        c = replicate 46 c ++ "\n"

{- create a new, empty Scrabble board -}
newListBoard :: ListBoard
newListBoard = fmap f (LM boardBonuses) where
  f (pos,b) = Square Nothing b pos

{- place a single tile, without worrying about scoring -}
putTileOnListBoard :: Pos p => ListBoard -> p -> Tile -> ListBoard
putTileOnListBoard (LM b) p t = LM (mapNth newRow (y p) b) where
  newRow = mapNth (\(Square _ b p) -> Square (Just t) b p) (x p)
  mapNth :: (a -> a) -> Int -> [a] -> [a]
  mapNth f i as = xs ++ [f $ head ys] ++ drop 1 ys
    where (xs,ys) = splitAt i as
