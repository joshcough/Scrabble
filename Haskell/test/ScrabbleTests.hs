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
{-
  These break the linker:
    Linking dist/build/tests/tests ...
    Undefined symbols for architecture x86_64:
      "_scrabzu9tVa6U7XyHd9TMpkN0oVXH_ScrabbleziMatrix_above_info", referenced from:

 testGroup "Bag Unit Tests" [
   u "# e's in bag"     $ countLettersInBag 'E' orderedBag   @?= 12
  ,u "# _'s in bag"     $ countLettersInBag '_' orderedBag   @?= 2
  ,u "# letters in bag" $ length orderedBag                  @?= 100
  ,u "# shuffled bag"   $ do { b <- newShuffledBag; length b @?= 100 }
  ,u "total points"     $ totalPoints                        @?= 187
  ,u "word points"      $ simpleWordPoints "XYZ"             @?= 22
  ]
  testGroup "Board Unit Tests" [
    u "?" $ Square Nothing W3 (Position 7 7) @?= Square Nothing W3 (Position 7 7)
   ,u "?" $ snd (quickPut [("ZOOLOGIC", Vertical, (0,0))]) @?= [7]
   ,u "?" $ snd (quickPut [("ZOOLOGIC", Vertical, (0,0))]) @?= [7]
  ],

  testGroup "Game Unit Tests" [
    u "?" $ do
      g1 <- newGame [human "Josh"]
      g2 <- newGame [human "Josh"]
      g1 @?= g2
  ] -}
 ]
