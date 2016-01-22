{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ScrabbleTests (tests) where

import Data.Monoid (mempty)
import Data.List
import Scrabble
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.TH
import Test.QuickCheck
import Test.QuickCheck.Instances.Char
import Test.HUnit
import TestHelpers

-- Bag Unit Tests
bag = orderedBag
case_sanity_check       = bag @?= orderedBag
case_es_in_bag          = countLettersInBag 'E' bag @?= 12
case_blanks_in_bag      = countLettersInBag '_' bag @?= 2
case_letters_in_bag     = length bag @?= 100
case_count_shuffled_bag = do { b <- newShuffledBag; length b @?= 100 }
case_total_points       = totalPoints @?= 187
case_word_points        = simpleWordPoints "XYZ" @?= 22

-- Board Unit Tests
case_lower_vertical     = snd (placeVert "zoologic") @?= [40]
case_upper_vertical     = snd (placeVert "ZOOLOGIC") @?= [40]
case_lower_horizonal    = snd (placeHztl "zoologic") @?= [40]
case_upper_horizonal    = snd (placeHztl "ZOOLOGIC") @?= [40]

-- Game Unit Tests
case_players_have_full_racks_at_the_start_of_a_new_game = do
  g <- newGame [human "Josh", human "Jimbo"]
  (length . playerRack <$> gamePlayers g) @?= [7,7]

{- places the given word vertically starting on
   the center position of a new board -}
placeVert :: String -> (ListBoard,[Score])
placeVert word = centered word Vertical

{- places the given word horizontally starting on
   the center position of a new board -}
placeHztl :: String -> (ListBoard,[Score])
placeHztl word = centered word Horizontal

{- places the given word on the center position of a new board
   using the given orientation -}
centered :: String -> Orientation -> (ListBoard,[Score])
centered word orientation = quickPut [(word, orientation, (7,7))]

tests = $testGroupGenerator