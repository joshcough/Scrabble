module Scrabble.Search where

import Data.Char (toLower, toUpper)
import System.Random.Shuffle
import Prelude hiding (Word, or, and, all)
import Scrabble.Types
import qualified Data.List as List

dictionary :: IO [String]
dictionary = lines <$> readFile "dict/en.txt"

type Search1 = Tray -> Word -> Bool

cheat :: Search1 -> Tray -> IO [Word]
cheat search tray = runSearch1 search tray <$> dictionary

-- Run a search on a whole dictionary of words
runSearch1 :: Search1 -> Tray -> [Word] -> [Word]
runSearch1 s t = filter (s t)

or :: Search1 -> Search1 -> Search1
or s1 s2 r w = s1 r w || s2 r w

and :: Search1 -> Search1 -> Search1
and s1 s2 r w = s1 r w && s2 r w

any' :: [Search1] -> Search1
any' = foldl or (\_ _ -> False)

all' :: [Search1] -> Search1
all' = foldl and (\_ _ -> True)

combine :: Bool -> (Search1 -> Search1 -> Search1) -> [Search1] -> Search1
combine b f = foldl f (\_ _ -> b)

any :: [Search1] -> Search1
any = combine False or

all :: [Search1] -> Search1
all = combine True and

lows :: String -> String
lows = List.sort . fmap toLower

-- TODO: be careful with what this means...
-- if there are dups in the tray, does the word also need dups?
containsAll :: Search1
containsAll = contains' List.all

containsAny :: Search1
containsAny = contains' List.any

contains' :: ((Char -> Bool) -> [Char] -> Bool) -> Search1
contains' f t w = f (\c -> elem c w') (lows t) where w' = lows w

containsOnly :: Search1
containsOnly t w = lows t == lows w

containsNone :: Search1
containsNone t = not . containsAny t

containsLetterAtPos :: Letter -> Int -> Search1
containsLetterAtPos l n _ w =
  if n - 1 > length w then False else w !! n == l

