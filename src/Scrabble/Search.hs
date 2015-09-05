module Scrabble.Search where

import Data.List hiding (or, and)
import System.Random.Shuffle
import Prelude hiding (Word, or, and)
import Scrabble.Types

dictionary = readFile "dict/en.txt"

type Search1 = Tray -> Word -> Bool

runSearch1 :: Tray -> Search1 -> [Word] -> [Word]
runSearch1 t s = filter (s t)

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


