{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DefaultSignatures #-}

module Scrabble.ScrabbleArbitrary where

import Control.Applicative
import qualified Data.List.NonEmpty as NE
import Data.Map
import Test.QuickCheck
import Scrabble
import TestHelpers

instance Arbitrary a => Arbitrary (NE.NonEmpty a) where
  arbitrary = NE.fromList <$> listOf1 arbitrary

instance Arbitrary PlayerType where
  arbitrary = oneof $ return <$> [ Human, AI ]

instance Arbitrary Letter where
  arbitrary = oneof $ return <$> [A .. Z]

instance Arbitrary Tile where
  arbitrary = fromLetter <$> arbitrary

instance Arbitrary Rack where
  arbitrary = Rack . (take 7) <$> shuffle orderedTiles

instance Arbitrary Bag where
  arbitrary = Bag <$> shuffle orderedTiles

instance Arbitrary Player where
  arbitrary = Player
                <$> arbitrary
                <*> arbitrary
                <*> (Rack <$> listOf arbitrary)
                <*> arbitrary
                <*> arbitrary -- TODO: the final arbitrary here could mean duplicate ids. fix.

instance Arbitrary TilePut where
  arbitrary = oneof [
    LetterTilePut <$> arbitrary <*> arbitrary,
    BlankTilePut  <$> arbitrary <*> arbitrary ]

instance Arbitrary WordPut where
  arbitrary = WordPut <$> arbitrary

-- strictly speaking this doesn't work because the turns aren't
-- applied to the board...but it's good to have for json testing
instance Arbitrary Game where
  arbitrary = Game <$> arbitrary     -- players
                   <*> pure newBoard -- board
                   <*> arbitrary     -- bag (shuffled)
                   <*> pure dict     -- always the same bag
                   <*> arbitrary     -- arbitrary turns...

instance Arbitrary Turn where
  arbitrary = Turn <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
