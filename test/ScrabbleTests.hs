{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ScrabbleTests (tests) where

import Data.Monoid (mempty)
import Data.List
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Test.QuickCheck.Instances.Char
import Test.HUnit

import UnitTests.TestHelpers

import Scrabble

-- TODO: get this working with template haskell.
-- https://hackage.haskell.org/package/test-framework-th
tests = [
  testGroup "Bag Unit Tests" [
    u "sanity check"     $ orderedBag @?= orderedBag,
    u "# e's in bag"     $ countLettersInBag 'E' orderedBag   @?= 12,
    u "# _'s in bag"     $ countLettersInBag '_' orderedBag   @?= 2,
    u "# letters in bag" $ length orderedBag                  @?= 100,
    u "# shuffled bag"   $ do { b <- newShuffledBag; length b @?= 100 },
    u "total points"     $ totalPoints                        @?= 187,
    u "word points"      $ simpleWordPoints "XYZ"             @?= 22
  ],
  testGroup "Board Unit Tests" [
    u "lower/vertical"  $ snd (placeVert "zoologic") @?= [40]
   ,u "upper/vertical"  $ snd (placeVert "ZOOLOGIC") @?= [40]
   ,u "lower/horizonal" $ snd (placeHztl "zoologic") @?= [40]
   ,u "upper/horizonal" $ snd (placeHztl "ZOOLOGIC") @?= [40]
  ],
  testGroup "Game Unit Tests" [
    u "players have full racks at the start of a new game" $ do
      g <- newGame [human "Josh", human "Jimbo"]
      (length . playerRack <$> gamePlayers g) @?= [7,7]
  ]
 ] -- leave this indented one space

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