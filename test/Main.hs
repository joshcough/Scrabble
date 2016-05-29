module Main where

import Test.Framework                 (defaultMainWithOpts)
import Test.Framework.Options         (TestOptions, TestOptions'(..))
import Test.Framework.Runners.Options (RunnerOptions, RunnerOptions'(..))

import qualified Scrabble.BagTests          as BagTests
import qualified Scrabble.BoardTests        as BoardTests
import qualified Scrabble.GameTests         as GameTests
import qualified Scrabble.Move.WordPutTests as WordPutTests
import qualified Scrabble.SearchTests       as SearchTests
import qualified Scrabble.TileTests         as TileTests
import qualified Scrabble.Move.MoveTests    as MoveTests

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
  , SearchTests.tests
  , TileTests.tests
  , WordPutTests.tests
  , MoveTests.tests]
