{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE RankNTypes #-}
module Scrabble.ScrabbleArbitrary where

import Control.Applicative
import Data.Map
import Test.QuickCheck
import Test.QuickCheck.Function
import Test.QuickCheck.Instances
import Scrabble

instance Arbitrary PlayerType where
  arbitrary = oneof $ return <$> [ Human, AI ]

instance Arbitrary Tile where
  arbitrary = oneOf orderedBag

instance Arbitrary Player where
  arbitrary =
    Player <$> arbitrary <*> arbitrary <*> listOf arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (Game a) where
  arbitrary = unsafeNewGame <$> listOf arbitrary

{-
this could be done better:

data Game b = Game {
  gamePlayers :: [Player],
  gameBoard   :: b Square,
  gameBag     :: Bag,
  gameDict    :: Dict }

instance Arbitrary a => Arbitrary (Game a) where
  arbitrary = Game <$> arbitrary <*> arbitrary <*> ...
-}