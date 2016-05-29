{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Scrabble.GameTests (tests) where

import Data.List.NonEmpty (fromList)
import Scrabble
import Scrabble.ScrabbleArbitrary()
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.TH
import Test.HUnit
import TestHelpers

-- Game Unit Tests
case_players_have_full_racks_at_the_start_of_a_new_game = do
  g <- newGame $ fromList [human "Josh", human "Jimbo"]
  (length . rackTiles . playerRack <$> gamePlayers g) @?= fromList [7,7]

-- TODO ======> profile these! they are really slow.
prop_game_eq_reflexive :: Game -> Bool
prop_game_eq_reflexive = eq_reflexive

prop_game_round_trip_json :: Game -> Bool
prop_game_round_trip_json = roundTripJSON

prop_turn_round_trip_json :: Turn -> Bool
prop_turn_round_trip_json = roundTripJSON

case_play_some_words = s @?= [14,14,13] where
  Right (_,s) = putManyWords standardValidation words newBoard dict
  words = [
    ("HELL", Vertical,   (7,7)),
    ("HAS",  Horizontal, (5,6)),
    ("AS",   Horizontal, (5,7)) ]

tests = $testGroupGenerator
