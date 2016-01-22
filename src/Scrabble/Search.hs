module Scrabble.Search where

import Control.Monad ((<=<))
import Data.Foldable hiding (and, or, all)
import Control.Monad (filterM)
import Data.Char (toUpper)
import Data.List (delete, foldl', sort, permutations, group, tails)
import qualified Data.List as List
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude hiding (Word, or, and, all)
import Scrabble.Types
import System.IO.Unsafe

type Search1 = Word -> Bool

------ Single String search functions ------

{- Search for _all_ of the letters in the first string (s1).
   If a letter appears more than once in s1, it must
   appear more than once in s2.
   More accurately:
     If a letter appears n times in s1,
     it must appear n' times in s2 where n' >= n
 -}
containsAll :: String -> Search1
containsAll s1 s2 = fst $ foldl' f (True, ups s2) (ups s1) where
  f (b,s) c = if elem c s then (b, delete c s) else (False,s)

containsAny :: String -> Search1
containsAny = contains' List.any . ups where
  contains' :: ((Char -> Bool) -> [Char] -> Bool) -> String -> Search1
  contains' f t w = f (\c -> elem c (ups w)) (ups t)

-- TODO: what are the semantics of contains only?
-- csu.foldRight(true) { (c,acc) => acc && wu.exists(_ == c) }
-- ABCABCABC `containsOnly` ABC => true
containsOnly :: String -> Search1
containsOnly t w = foldl f True (ups t) where
  w' = ups w
  f acc c = elem c w'

containsNone :: String -> Search1
containsNone t = not . containsAny t

containsLetterAtPos :: Letter -> Int -> Search1
containsLetterAtPos l n w =
  if n >= length w then False else w !! n == l

endsWith :: String -> Search1
endsWith s w = startsWith (reverse s) (reverse w)

startsWith :: String -> Search1
startsWith s w = take (length s) w == s

looksLike :: String -> Search1
looksLike p w = error "todo"

regex :: String -> Search1
regex r w = error "todo"

ups :: String -> String
ups = List.sort . fmap toUpper

downs :: String -> String
downs = List.sort . fmap toUpper

------ Search combinators ------

or :: Search1 -> Search1 -> Search1
or s1 s2 w = s1 w || s2 w

and :: Search1 -> Search1 -> Search1
and s1 s2 w = s1 w && s2 w

any' :: [Search1] -> Search1
any' = foldr or (const False)

all' :: [Search1] -> Search1
all' = foldr and (const True)

combine :: Bool                ->
           (Search1            ->
           Search1 -> Search1) ->
           [Search1]           ->
           Search1
combine b f = foldr f (const b)

any :: [Search1] -> Search1
any = combine False or

all :: [Search1] -> Search1
all = combine True and

none :: [Search1] -> Search1
none ss w = not (all ss w)

matchAll  = Scrabble.Search.all
matchAny  = Scrabble.Search.any
matchNone = Scrabble.Search.none

------ Dictionary Searching ---------

dictionary :: IO Dict
dictionary = do
  d <- readFile "./dict/en.txt"
  return $ Set.fromList (fmap toUpper <$> lines d)

dictionaryUnsafe = unsafePerformIO dictionary

-- Run a search on a whole dictionary of words
runSearch1 :: Search1 -> Set Word -> Set Word
runSearch1 = Set.filter

cheat :: Search1 -> IO (Set Word)
cheat search = runSearch1 search <$> dictionary

dictContainsWord :: Dict -> Word -> Bool
dictContainsWord d = flip Set.member d . fmap toUpper

powerset :: [a] -> [[a]]
powerset = filterM (const [True, False])

searchDictForAllWords :: Dict -> Set String -> Set String
searchDictForAllWords d s = Set.intersection d s

searchDictForPowerset :: Dict -> [Char] -> Set String
searchDictForPowerset d = searchDictForAllWords d . permset

searchDictForPowersetSorted :: Dict -> [Char] -> [String]
searchDictForPowersetSorted d s =
  sort . Set.toList $ searchDictForPowerset d s

permset :: [Char] -> Set String
permset s = Set.fromList $ concat (permutations <$> powerset s)

{- A quick utility to search the dictionary
   for all possible words make with the given rack. -}
testSearch :: [Char] -> IO [Word]
testSearch rack = do
  d <- dictionary
  return $ searchDictForPowersetSorted d rack

shuffle xs [] = [xs]
shuffle [] ys = [ys]
shuffle (x:xs) (y:ys) =
  map (x:) (shuffle xs (y:ys))
    ++ map (y:) (shuffle (x:xs) ys)

anagrams = foldrM shuffle "" . group . sort

subanagrams = foldrM f "" . map tails . group . sort where
  f is j = is >>= flip shuffle j
