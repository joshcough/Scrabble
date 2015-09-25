{-# LANGUAGE TemplateHaskell #-}
module UnitTests.TestHelpers where

import Data.Monoid (mempty)
import Data.List
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Test.QuickCheck.Instances.Char
import Test.HUnit

p s t = testProperty (s ++ "property") t
u s t = testCase (s ++ "unit_test") t

props g ps = testGroup g $ f <$> ps where f (name,prop) = testProperty name prop
units g us = testGroup g $ f <$> us where f (name,test) = testCase     name test

allEqual :: (Eq a, Show a) => [a] -> IO ()
allEqual (x1:x2:xs) = (x1 @?= x2) >> allEqual (x2:xs)
allEqual _          = return ()

