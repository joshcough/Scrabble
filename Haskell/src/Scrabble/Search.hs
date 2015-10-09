module Scrabble.Search where

import Control.Monad (filterM)
import Data.Char (toUpper)
import Data.List (delete, foldl')
import qualified Data.List as List
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude hiding (Word, or, and, all)
import Scrabble.Types
import System.Random.Shuffle

dictionary :: IO Dict
dictionary = do
  d <- readFile "../dict/en.txt"
  return $ Set.fromList (ups <$> lines d)

-- TODO: should this be someplace else?
-- maybe this file just needs reorganization.
dictContainsWord :: Dict -> Word -> Bool
dictContainsWord d = flip Set.member d . ups

type Search1 = Word -> Bool

cheat :: Search1 -> IO (Set Word)
cheat search = runSearch1 search <$> dictionary

-- Run a search on a whole dictionary of words
runSearch1 :: Search1 -> Set Word -> Set Word
runSearch1 s = Set.filter s

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

ups :: String -> String
ups = List.sort . fmap toUpper

downs :: String -> String
downs = List.sort . fmap toUpper

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

containsOnly :: String -> Search1
containsOnly t w = ups t == ups w

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
