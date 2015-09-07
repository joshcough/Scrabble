module Scrabble.Search where

import Data.Char (toLower, toUpper)
import Data.List (delete)
import System.Random.Shuffle
import Prelude hiding (Word, or, and, all)
import Scrabble.Types
import qualified Data.List as List
import Debug.Trace

dictionary :: IO [String]
dictionary = lines <$> readFile "../dict/en.txt"

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

none :: [Search1] -> Search1
none ss w = not (all ss w)

matchAll  = Scrabble.Search.all
matchAny  = Scrabble.Search.any
matchNone = Scrabble.Search.none

lows :: String -> String
lows = List.sort . fmap toLower

-- TODO: be careful with what this means...
-- if there are dups in arg1, there must also be dups in arg2
-- are all the chars in s1 also in s2?
containsAll :: String -> Search1
containsAll s1 s2 = fst $ foldl f (True, s2) s1 where
  f :: (Bool, String) -> Char -> (Bool, String)
  f (b,s) c = if elem c s then (b, delete c s) else (False,s)

containsAny :: String -> Search1
containsAny = contains' List.any

contains' :: ((Char -> Bool) -> [Char] -> Bool) -> String -> Search1
contains' f t w = f (\c -> elem c w') (lows t) where w' = lows w

containsOnly :: String -> Search1
containsOnly t w = lows t == lows w

containsNone :: String -> Search1
containsNone t = not . containsAny t

containsLetterAtPos :: Letter -> Int -> Search1
containsLetterAtPos l n w = if n >= length w then False else w !! n == l

endsWith :: String -> Search1
endsWith s w = startsWith (reverse s) (reverse w)

startsWith :: String -> Search1
startsWith s w = take (length s) w == s

looksLike :: String -> Search1
looksLike p w = error "todo"

regex :: String -> Search1
regex r w = error "todo"
