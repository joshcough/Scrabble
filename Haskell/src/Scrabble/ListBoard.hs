{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Scrabble.ListBoard where

import Data.Char (toUpper)
import Data.Either (isRight)
import Data.List (foldl', intercalate)
import Data.Maybe (Maybe)
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import Debug.Trace
import qualified Data.Set as Set
import Prelude hiding (Word)
import Scrabble.Bag
import Scrabble.Board
import Scrabble.Matrix
import Scrabble.Types

type ListBoard  = ListMatrix Square

instance Board ListMatrix where
  putTile   = putTileOnListBoard
  showBoard = showListBoard
  newBoard  = newListBoard

showListBoard :: Bool -> ListBoard -> String
showListBoard printBonuses (LM board) = top ++ showRows ++ bottom where
  showRows      = intercalate "\n" (fmap showRow board) ++ "\n"
  showRow     r = "|" ++ concat (fmap showSquare' r)
  showSquare' s = showSquare printBonuses s ++ "|"
  top           = line '_'
  bottom        = line '-'
  line        c = replicate 46 c ++ "\n"

printListBoard :: Bool -> ListBoard -> IO ()
printListBoard b = putStrLn . showListBoard b

{- create a new, empty Scrabble board -}
newListBoard :: ListBoard
newListBoard = fmap f (LM boardBonuses) where
  f (pos,b) = Square Nothing b pos

{- place a single tile, without worrying about scoring -}
putTileOnListBoard :: Pos p => ListBoard -> p -> Tile -> ListBoard
putTileOnListBoard (LM b) p t = LM (mapNth newRow (y p) b) where
  newRow = mapNth (\(Square _ b p) -> Square (Just t) b p) (x p)
  mapNth :: (a -> a) -> Int -> [a] -> [a]
  mapNth f i as = xs ++ [f $ head ys] ++ drop 1 ys where (xs,ys) = splitAt i as

{- put some words on a brand new board -}
quickPut :: [(String, Orientation, (Int, Int))] -> (ListBoard,[Score])
quickPut words = either error id  $ quickPut' words newBoard

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
