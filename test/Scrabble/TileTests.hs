{-# LANGUAGE TemplateHaskell #-}

module Scrabble.TileTests where

import Scrabble.ScrabbleArbitrary()
import Scrabble.Tile
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.TH
import Test.HUnit
import TestHelpers

import qualified Data.Map.Strict as Map

case_tile_equality           = [A .. Z] @?= [A .. Z]
case_word_points_A_through_Z = simpleWordPoints [A .. Z] @?= 87
case_word_points_X_Y_Z       = simpleWordPoints [X,Y,Z]  @?= 22
case_tiles_to_string         = tilesToString (fromLetter <$> [A .. Z]) @?= ['A'..'Z']
case_points_get_Z            = Map.lookup Z     points @?= Just 10
case_points_get_Blank        = Map.lookup Blank points @?= Just 0

prop_tile_round_trip_json :: Tile -> Bool
prop_tile_round_trip_json = roundTripJSON

tests = $testGroupGenerator
