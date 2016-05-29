{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Scrabble.BagTests where

import Scrabble
import Scrabble.ScrabbleArbitrary()
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.TH
import Test.HUnit
import TestHelpers

bag = orderedBag

case_sanity_check       = bag @?= orderedBag
case_es_in_bag          = countLettersInBag E bag @?= 12
case_blanks_in_bag      = countLettersInBag Blank bag @?= 2
case_letters_in_bag     = bagSize bag @?= 100
case_count_shuffled_bag = do { b <- newShuffledBag; bagSize b @?= 100 }
case_total_points       = do { b <- newShuffledBag; pointsInBag b @?= 187 }

prop_rack_round_trip_json :: Rack -> Bool
prop_rack_round_trip_json = roundTripJSON

prop_bag_round_trip_json :: Bag -> Bool
prop_bag_round_trip_json = roundTripJSON

tests = $testGroupGenerator
