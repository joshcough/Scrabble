{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Scrabble.BagTests where

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

tests = $testGroupGenerator