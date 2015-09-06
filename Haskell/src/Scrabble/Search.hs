module Scrabble.Search where

import Data.Char (toLower, toUpper)
import System.Random.Shuffle
import Prelude hiding (Word, or, and, all)
import Scrabble.Types
import qualified Data.List as List
import Debug.Trace

dictionary :: IO [String]
dictionary = lines <$> readFile "dict/en.txt"

type Search1 = Word -> Bool

cheat :: Search1 -> IO [Word]
cheat search = runSearch1 search <$> dictionary

-- Run a search on a whole dictionary of words
runSearch1 :: Search1 -> [Word] -> [Word]
runSearch1 s = filter s

or :: Search1 -> Search1 -> Search1
or s1 s2 w = s1 w || s2 w

and :: Search1 -> Search1 -> Search1
and s1 s2 w = s1 w && s2 w

any' :: [Search1] -> Search1
any' = foldl or (const False)

all' :: [Search1] -> Search1
all' = foldl and (const True)

combine :: Bool -> (Search1 -> Search1 -> Search1) -> [Search1] -> Search1
combine b f = foldl f (const b)

any :: [Search1] -> Search1
any = combine False or

all :: [Search1] -> Search1
all = combine True and

lows :: String -> String
lows = List.sort . fmap toLower

-- TODO: be careful with what this means...
-- if there are dups in the input, does the word also need dups?
containsAll :: Tray -> Search1
containsAll = contains' List.all

containsAny :: Tray -> Search1
containsAny = contains' List.any

contains' :: ((Char -> Bool) -> [Char] -> Bool) -> Tray -> Search1
contains' f t w = f (\c -> elem c w') (lows t) where w' = lows w

containsOnly :: Tray -> Search1
containsOnly t w = lows t == lows w

containsNone :: Tray -> Search1
containsNone t = not . containsAny t

containsLetterAtPos :: Letter -> Int -> Search1
containsLetterAtPos l n w = if n >= length w then False else w !! n == l

