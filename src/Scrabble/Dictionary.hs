{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

-- | Letter, Word, and Dictionary representation
module Scrabble.Dictionary
  (
    Word
  , Dict
  , Letter(..)
  , HasLetter(..)
  , dictContainsPrefix
  , dictContainsWord
  , englishDictionary
  , findWords
  , letterFromChar
  , toChar
  , unsafeReadEnglishDictionary
  , wordFromString
  , wordToString
  ) where

import Data.Aeson       (ToJSON, FromJSON)
import Data.Char        (toUpper)
import Data.List        (delete, inits)
import Data.Map         (Map)
import Data.Set         (Set)
import Data.Tuple       (swap)
import GHC.Generics
import Prelude hiding   (Word)
import System.IO.Unsafe

import qualified Data.Maybe as Maybe
import qualified Data.Map   as Map
import qualified Data.Set   as Set

{- ===== Letters ===== -}

data Letter =
  A | B | C | D | E | F | G | H | I | J | K | L | M |
  N | O | P | Q | R | S | T | U | V | W | X | Y | Z | Blank
  deriving (Enum, Eq, Ord, Generic, ToJSON, FromJSON)

instance Show Letter where
  show l = [toChar l]

-- | Convert a Char to a Letter, if the Char is a valid Letter (A-Z).
letterFromChar :: Char -> Maybe Letter
letterFromChar c = Map.lookup c charToLetterMap

-- | Convert a Letter back into a Char. Always valid.
toChar :: Letter -> Char
toChar l = Maybe.fromJust $ Map.lookup l letterToCharMap

-- private value.
letterToCharList :: [(Letter,Char)]
letterToCharList = [
  (A, 'A'), (B, 'B'), (C, 'C'), (D, 'D'), (E, 'E'), (F, 'F'), (G, 'G'),
  (H, 'H'), (I, 'I'), (J, 'J'), (K, 'K'), (L, 'L'), (M, 'M'), (N, 'N'),
  (O, 'O'), (P, 'P'), (Q, 'Q'), (R, 'R'), (S, 'S'), (T, 'T'), (U, 'U'),
  (V, 'V'), (W, 'W'), (X, 'X'), (Y, 'Y'), (Z, 'Z'), (Blank, '_') ]

-- private value.
letterToCharMap :: Map Letter Char
letterToCharMap = Map.fromList letterToCharList

-- private value.
charToLetterMap :: Map Char Letter
charToLetterMap = Map.fromList (swap <$> letterToCharList)

{- ===== Words ===== -}

type Word = [Letter]

wordToString :: Word -> String
wordToString = fmap toChar

wordFromString :: String -> Maybe Word
wordFromString s = sequence $ letterFromChar <$> s

class HasLetter a where
  letter :: a -> Letter

{- ===== Dictionary ===== -}

data Dict = Dict {
   dictWords    :: Set Word -- ^ All the words in the dictionary
 , dictPrefixes :: Set Word -- ^ The prefixes of all the words in the dictionary.
} deriving Eq

instance Show Dict where
  show d = concat ["(Dict ", nrWords, ", ", nrPrefixes, ")"] where
    nrWords    = "words: "    ++ show (length $ dictWords d)
    nrPrefixes = "prefixes: " ++ show (length $ dictPrefixes d)

-- | Returns true if the dict contains the given word
dictContainsWord :: Dict -> Word -> Bool
dictContainsWord = flip Set.member . dictWords

-- | Returns true if the dict contains the given prefix
dictContainsPrefix :: Dict -> Word -> Bool
dictContainsPrefix = flip Set.member . dictPrefixes

-- Reads in a dictionary of Scrabble words from the given file.
readDictionary :: FilePath -> IO Dict
readDictionary dict = mkDict <$> readFile dict where
  mkDict :: String -> Dict
  mkDict = uncurry dictFromLists . wordsAndPrefixes
  wordsAndPrefixes :: String -> ([Word], [[Word]])
  wordsAndPrefixes dict = unzip $ wordWithPrefixes <$> lines dict where
    -- return the word, and all its prefixes
    wordWithPrefixes :: String -> (Word, [Word])
    wordWithPrefixes w =
      -- first turn each char into a Letter, when create result
      let w' = Maybe.fromJust . letterFromChar . toUpper <$> w
      in (w', init $ inits w')
  -- list based constructor for Dict
  dictFromLists :: [Word] -> [[Word]] -> Dict
  dictFromLists wordList prefixesLists =
    Dict (Set.fromList wordList) (Set.fromList $ concat prefixesLists)

-- | Find all the words in the dictionary that can
--   be made with the given letters
findWords :: Dict -> [Letter] -> Set Word
findWords dict = Set.fromList . concat . findWords' [] where
  findWords' :: Word -> [Letter] -> [[Word]]
  findWords' prefix rest = [next c : recur c | c <- rest, lookup (next c)]
    where next  c = prefix ++ [c]
          recur c = concat (findWords' (next c) (delete c rest))
          lookup  = dictContainsPrefix dict

{- ===== English Dictionary ===== -}

-- English dictionary file path
englishDictionaryPath :: FilePath
englishDictionaryPath = "./dict/en.txt"

-- | Reads in the (English) dictionary of Scrabble words.
englishDictionary :: IO Dict
englishDictionary = readDictionary englishDictionaryPath

-- | Read the English dictionary (performing the IO action)
unsafeReadEnglishDictionary :: Dict
unsafeReadEnglishDictionary = unsafePerformIO englishDictionary
