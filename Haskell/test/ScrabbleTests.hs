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

string = listOf lowerAlpha

-- TODO: get this working with template haskell.
-- https://hackage.haskell.org/package/test-framework-th
tests = [
  testGroup "Search Properties" [
    p "startsWith self"     . forAll string $ \s -> startsWith  s s
   ,p "containsAll self"    . forAll string $ \s -> containsAll s s
   ,p "containsAll reverse" . forAll string $ \s -> containsAll s (reverse s)
   ,p "containsAll implies containsAny" .
      forAll string $ \s -> containsAll  "ab" s ==> containsAny "ab" s
   ,p "containsNone implies ! containsAny" .
      forAll string $ \s -> containsNone "ab" s ==> not $ containsAny "ab" s
  ],

  testGroup "Search Unit Tests" [
    u "startsWith"   $ startsWith   "hello" "hello, world"   @?= True
   ,u "endsWith"     $ endsWith     "world" "hello, world"   @?= True
   ,u "containsOnly" $ containsOnly "abc"   "abc"            @?= True
  ],

 testGroup "Bag Unit Tests" [
   u "# e's in bag"     $ countLettersInBag 'E' orderedBag   @?= 12
  ,u "# _'s in bag"     $ countLettersInBag '_' orderedBag   @?= 2
  ,u "# letters in bag" $ length orderedBag                  @?= 100
  ,u "# shuffled bag"   $ do { b <- newShuffledBag; length b @?= 100 }
  ,u "total points"     $ totalPoints                        @?= 187
  ,u "word points"      $ simpleWordPoints "XYZ"             @?= 22
  ]

{-
  testGroup "Board Unit Tests" [
   These break the linker:
      Linking dist/build/tests/tests ...
      Undefined symbols for architecture x86_64:
        "_scrabzu9tVa6U7XyHd9TMpkN0oVXH_ScrabbleziMatrix_above_info", referenced from:
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
