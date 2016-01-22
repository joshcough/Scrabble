module Main where

import Data.Monoid (mempty)
import Test.Framework (defaultMain, defaultMainWithOpts, testGroup)
import Test.Framework.Options (TestOptions, TestOptions'(..))
import Test.Framework.Runners.Options (RunnerOptions, RunnerOptions'(..))
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck
import Test.HUnit

import Data.List
import ScrabbleTests
import SearchTests
import Scrabble

--For a complete example, see: http://bit.ly/1G5MIoq
--also: https://hackage.haskell.org/package/test-framework-th
main = do
  defaultMainWithOpts allTests $ (mempty :: RunnerOptions) {
    ropt_test_options = Just $ (mempty :: TestOptions) {
      topt_maximum_generated_tests = Just 100
    }
  }

allTests =
  [ScrabbleTests.tests,
   SearchTests.tests]