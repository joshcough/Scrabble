{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Scrabble.GameTests (tests) where

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

-- Game Unit Tests
case_players_have_full_racks_at_the_start_of_a_new_game = do
  g <- newGame [human "Josh", human "Jimbo"]
  (length . playerRack <$> gamePlayers g) @?= [7,7]

tests = $testGroupGenerator