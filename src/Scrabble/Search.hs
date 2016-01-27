module Scrabble.Search (
  Search
 ,all
 ,and
 ,Scrabble.Search.any
 ,containsAll
 ,containsAny
 ,containsNone
 ,containsOnly
 ,containsLetterAtPos
 ,dictionary
 ,endsWith
 ,looksLike
 ,matchAll
 ,matchAny
 ,matchNone
 ,none
 ,or
 ,regex
 ,runSearch
 ,searchWordBagForPowersetSorted
 ,startsWith
 ,testSearch
 ,wordBagContainsWord
) where

import Control.Monad ((<=<))
import Data.Foldable hiding (and, or, all)
import Control.Monad (filterM)
import Data.Char (toUpper)
import Data.List (delete, foldl', sort, permutations, group, tails)
import qualified Data.List as List
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude hiding (or, and, all)
import System.IO.Unsafe

type Search = String -> Bool

------ Single String search functions ------

{- Search for _all_ of the letters in the first string (s1).
   If a character appears more than once in s1, it must
   appear more than once in s2.
   More accurately:
     If a character appears n times in s1,
     it must appear n' times in s2 where n' >= n
 -}
containsAll :: String -> Search
containsAll s1 s2 = fst $ foldl' f (True, ups s2) (ups s1) where
  f (b,s) c = if elem c s then (b, delete c s) else (False,s)

containsAny :: String -> Search
containsAny = contains' List.any . ups where
  contains' :: ((Char -> Bool) -> [Char] -> Bool) -> String -> Search
  contains' f t w = f (\c -> elem c (ups w)) (ups t)

-- TODO: what are the semantics of contains only?
-- csu.foldRight(true) { (c,acc) => acc && wu.exists(_ == c) }
-- ABCABCABC `containsOnly` ABC => true
containsOnly :: String -> Search
containsOnly t w = foldl f True (ups t) where
  w' = ups w
  f acc c = elem c w'

containsNone :: String -> Search
containsNone t = not . containsAny t

containsLetterAtPos :: Char -> Int -> Search
containsLetterAtPos l n w =
  if n >= length w then False else w !! n == l

endsWith :: String -> Search
endsWith s w = startsWith (reverse s) (reverse w)

startsWith :: String -> Search
startsWith s w = take (length s) w == s

looksLike :: String -> Search
looksLike p w = error "todo"

regex :: String -> Search
regex r w = error "todo"

ups :: String -> String
ups = List.sort . fmap toUpper

downs :: String -> String
downs = List.sort . fmap toUpper

------ Search combinators ------

or :: Search -> Search -> Search
or s1 s2 w = s1 w || s2 w

and :: Search -> Search -> Search
and s1 s2 w = s1 w && s2 w

any' :: [Search] -> Search
any' = foldr or (const False)

all' :: [Search] -> Search
all' = foldr and (const True)

combine :: Bool                         ->
           (Search -> Search -> Search) ->
           [Search]                     ->
           Search
combine b f = foldr f (const b)

any :: [Search] -> Search
any = combine False or

all :: [Search] -> Search
all = combine True and

none :: [Search] -> Search
none ss w = not (all ss w)

matchAll  = Scrabble.Search.all
matchAny  = Scrabble.Search.any
matchNone = Scrabble.Search.none

------ Dictionary Searching ---------

dictionary :: IO (Set String)
dictionary = do
  d <- readFile "./dict/en.txt"
  return $ Set.fromList (fmap toUpper <$> lines d)

dictionaryUnsafe = unsafePerformIO dictionary

-- Run a search on a whole bag of words
runSearch :: Search -> Set String -> Set String
runSearch = Set.filter

cheat :: Search -> IO (Set String)
cheat search = runSearch search <$> dictionary

wordBagContainsWord :: Set String -> String -> Bool
wordBagContainsWord d = flip Set.member d . fmap toUpper

powerset :: [a] -> [[a]]
powerset = filterM (const [True, False])

searchWordBagForAllWords :: Set String -> Set String -> Set String
searchWordBagForAllWords d s = Set.intersection d s

searchWordBagForPowerset :: Set String -> [Char] -> Set String
searchWordBagForPowerset d = searchWordBagForAllWords d . permset

searchWordBagForPowersetSorted :: Set String -> [Char] -> [String]
searchWordBagForPowersetSorted d s =
  sort . Set.toList $ searchWordBagForPowerset d s

permset :: [Char] -> Set String
permset s = Set.fromList $ concat (permutations <$> powerset s)

{- A quick utility to search the dict
   for all possible words make with the given rack. -}
testSearch :: [Char] -> IO [String]
testSearch rack = do
  d <- dictionary
  return $ searchWordBagForPowersetSorted d rack

shuffle xs [] = [xs]
shuffle [] ys = [ys]
shuffle (x:xs) (y:ys) =
  map (x:) (shuffle xs (y:ys))
    ++ map (y:) (shuffle (x:xs) ys)

anagrams = foldrM shuffle "" . group . sort

subanagrams = foldrM f "" . map tails . group . sort where
  f is j = is >>= flip shuffle j
