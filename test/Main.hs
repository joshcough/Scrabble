module Main where

import Data.List
import Data.Monoid (mempty)
import Test.Framework (defaultMain, defaultMainWithOpts, testGroup)
import Test.Framework.Options (TestOptions, TestOptions'(..))
import Test.Framework.Runners.Options (RunnerOptions, RunnerOptions'(..))
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit
import Test.QuickCheck

import Scrabble
import qualified Scrabble.BagTests      as BagTests
import qualified Scrabble.BoardTests    as BoardTests
import qualified Scrabble.GameTests     as GameTests
import qualified Scrabble.ScrabbleTests as ScrabbleTests
import qualified Scrabble.SearchTests   as SearchTests
import qualified Scrabble.TileTests     as TileTests

--For a complete example, see: http://bit.ly/1G5MIoq
--also: https://hackage.haskell.org/package/test-framework-th
main = do
  defaultMainWithOpts allTests $ (mempty :: RunnerOptions) {
    ropt_test_options = Just $ (mempty :: TestOptions) {
      topt_maximum_generated_tests = Just 100
    }
  }

allTests =
  [ BagTests.tests
  , BoardTests.tests
  , GameTests.tests
  , ScrabbleTests.tests
  , SearchTests.tests
  , TileTests.tests ]