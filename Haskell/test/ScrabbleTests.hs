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
tests = [ testGroup "Scrabble Tests" [
  p "example"     (\(xs::String) -> not (null xs) ==> head (sort xs) == minimum xs),
  p "starts_with" (forAll (listOf lowerAlpha) (\c -> startsWith c c)),
  u "starts_with" (startsWith "hello" "hello, world" @?= True)
 ]]
