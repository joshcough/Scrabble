{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

{-
module Scrabble.ArrayBoard where

import Data.Array
import Data.List (cycle)

data Count =
    Zero | One    | Two    | Three    | Four
  | Five | Six    | Seven  | Eight    | Nine
  | Ten  | Eleven | Twelve | Thirteen | Fourteen
  deriving (Read, Show, Eq, Ord, Enum, Bounded, Ix)

newtype ScrabbleBoard a = ScrabbleBoard (Array (Count, Count) a)
  deriving (Functor, Foldable, Traversable, Show)

newScrabbleBoard :: a -> ScrabbleBoard a
newScrabbleBoard a = ScrabbleBoard $
  listArray ((Zero,Zero), (Fourteen,Fourteen)) (cycle [a])

instance Pos (Count, Count) where
  coors      = error "never implement coors for count"
  x          = error "never implement x for count"
  y          = error "never implement coors for count"
  aboveP     (x,y) = (x, pred y)
  belowP     (x,y) = (x, succ y)
  leftOfP    (x,y) = (pred x, y)
  rightOfP   (x,y) = (succ x, y)

--listArray :: (IArray a e, Ix i) => (i, i) -> [e] -> a i e
instance Matrix ScrabbleBoard where
  type Row ScrabbleBoard = Array Count
  type MIx ScrabbleBoard = Count
  elemAt  b p = Just (elemAtT b p)
  row     b y = Just (rowT b y)
  col     b x = Just (colT b x)
  rows    b                   = rowT b <$> [minBound..maxBound]
  cols    b                   = colT b <$> [minBound..maxBound]
  above   (ScrabbleBoard a) (x,y) = listArray (minBound, pred y) (\c -> a ! (x,c) <$> [minBound..pred y])
  below   (ScrabbleBoard a) (x,y) = listArray (succ y, maxBound) (\c -> a ! (x,c) <$> [succ y..maxBound])
  leftOf  (ScrabbleBoard a) (x,y) = listArray (minBound, pred x) (\c -> a ! (c,y) <$> [minBound..pred x])
  rightOf (ScrabbleBoard a) (x,y) = listArray (succ x, maxBound) (\c -> a ! (c,y) <$> [succ x..maxBound])

instance TotalMatrix ScrabbleBoard where
  elemAtT (ScrabbleBoard a) p = a ! p
  rowT    (ScrabbleBoard a) y = listArray (minBound, maxBound) (\c -> a ! (c,y)  <$> [minBound..maxBound])
  colT    (ScrabbleBoard a) x = listArray (minBound, maxBound) (\c -> a ! (x,c)  <$> [minBound..maxBound])

-}