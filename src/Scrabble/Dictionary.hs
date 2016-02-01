{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

-- | Letter and Dictionary representation
module Scrabble.Dictionary (
  Word
 ,Dict
 ,Letter(..)
 ,HasLetter(..)
 ,dictContainsWord
 ,dictionary
 ,dictionaryUnsafe
 ,fromChar
 ,wordFromString
 ,toChar
 ,toString
) where

import Data.Aeson (ToJSON, FromJSON)
import Data.Char (toUpper)
import qualified Data.Maybe as Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple (swap)
import GHC.Generics
import Prelude hiding (Word)
import System.IO.Unsafe

data Letter =
  A | B | C | D | E | F | G | H | I | J | K | L | M |
  N | O | P | Q | R | S | T | U | V | W | X | Y | Z | Blank
  deriving (Enum, Eq, Ord, Generic, ToJSON, FromJSON)

instance Show Letter where
  show l = [toChar l]

toCharList :: [(Letter,Char)]
toCharList = [
  (A, 'A'), (B, 'B'), (C, 'C'), (D, 'D'), (E, 'E'), (F, 'F'), (G, 'G'),
  (H, 'H'), (I, 'I'), (J, 'J'), (K, 'K'), (L, 'L'), (M, 'M'), (N, 'N'),
  (O, 'O'), (P, 'P'), (Q, 'Q'), (R, 'R'), (S, 'S'), (T, 'T'), (U, 'U'),
  (V, 'V'), (W, 'W'), (X, 'X'), (Y, 'Y'), (Z, 'Z'), (Blank, '_') ]

fromCharList :: [(Char,Letter)]
fromCharList = swap <$> toCharList

toCharMap :: Map Letter Char
toCharMap = Map.fromList toCharList

fromCharMap :: Map Char Letter
fromCharMap = Map.fromList fromCharList

toChar :: Letter -> Char
toChar l = Maybe.fromJust $ Map.lookup l toCharMap

fromChar :: Char -> Maybe Letter
fromChar c = Map.lookup c fromCharMap

type Word = [Letter]

toString :: Word -> String
toString = fmap toChar

wordFromString :: String -> Maybe Word
wordFromString s = sequence $ fromChar <$> s

class HasLetter a where
  letter :: a -> Letter

------ Dictionary Searching ---------
type Dict = Set Word

dictionary :: IO Dict
dictionary = do
  d <- readFile "./dict/en.txt"
  return $ Set.fromList (fmap f <$> lines d) where
  f = Maybe.fromJust . fromChar . toUpper

dictionaryUnsafe :: Dict
dictionaryUnsafe = unsafePerformIO dictionary

dictContainsWord :: Dict -> Word -> Bool
dictContainsWord d = flip Set.member d
