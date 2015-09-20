module Scrabble.Search where

import Data.Char (toUpper)
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

ups :: String -> String
ups = List.sort . fmap toUpper

{- Search for _all_ of the letters in the first string (s1).
   If a letter appears more than once in s1, it must
   appear more than once in s2.
   More accurately:
     If a letter appears n times in s1,
     it must appear n' times in s2 where n' >= n
 -}
containsAll :: String -> Search1
containsAll s1 s2 = fst $ foldl f (True, ups s2) (ups s1) where
  f (b,s) c = if elem c s then (b, delete c s) else (False,s)

containsAny :: String -> Search1
containsAny = contains' List.any . ups

contains' :: ((Char -> Bool) -> [Char] -> Bool) -> String -> Search1
contains' f t w = f (\c -> elem c w') (ups t) where w' = ups w

containsOnly :: String -> Search1
containsOnly t w = ups t == ups w

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
