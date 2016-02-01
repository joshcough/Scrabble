{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DefaultSignatures #-}

module Scrabble.ScrabbleArbitrary where

import Control.Applicative
import Data.Map
import Test.QuickCheck
import Scrabble
import TestHelpers

instance Arbitrary PlayerType where
  arbitrary = oneof $ return <$> [ Human, AI ]

instance Arbitrary Letter where
  arbitrary = oneof $ return <$> [A .. Z]

instance Arbitrary Tile where
  arbitrary = fromLetter <$> arbitrary

instance Arbitrary Bag where
  arbitrary = Bag <$> shuffle orderedTiles

instance Arbitrary Player where
  arbitrary =
    Player <$> arbitrary <*> arbitrary <*> listOf arbitrary <*> arbitrary

instance Arbitrary Game where
  arbitrary = Game <$> listOf1 arbitrary -- players
                   <*> pure newBoard     -- board
                   <*> arbitrary         -- bag (shuffled)
                   <*> pure dict         -- always the same bag
                   <*> pure []           -- turns ... TODO: consider something here?
