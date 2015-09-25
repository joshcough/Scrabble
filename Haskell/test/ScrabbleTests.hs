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
      forAll string $ \s -> containsAll "abc"  s ==> containsAny "abc" s
   ,p "containsNone implies ! containsAny" .
      forAll string $ \s -> containsNone "abc" s ==> not (containsAny "abc" s)
  ],

  testGroup "Search Unit Tests" [
    u "startsWith" $ startsWith "hello" "hello, world" @?= True
   ,u "endsWith"   $ endsWith   "world" "hello, world" @?= True
   ,u "containsOnly" $ containsOnly "abc" "abc" @?= True
  ],

 testGroup "Bag Properties" [
   p "startsWith" . forAll (listOf lowerAlpha) $ \c -> startsWith c c
  ,u "startsWith" $ startsWith "hello" "hello, world" @?= True
  ,u "letter pos" $ containsLetterAtPos 'c' 2 "abcde" @?= True
  ,u "letter pos" $ containsLetterAtPos 'c' 3 "abcde" @?= False
  ]
 ]
