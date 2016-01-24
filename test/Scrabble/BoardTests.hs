{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Scrabble.BoardTests (tests) where

import Data.Monoid (mempty)
import Data.List
import Scrabble
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.TH
import Test.QuickCheck
import Test.QuickCheck.Instances.Char
import Test.HUnit
import TestHelpers

-- Board Unit Tests
case_lower_vertical     = snd (placeVert "zoologic") @?= [40]
case_upper_vertical     = snd (placeVert "ZOOLOGIC") @?= [40]
case_lower_horizonal    = snd (placeHztl "zoologic") @?= [40]
case_upper_horizonal    = snd (placeHztl "ZOOLOGIC") @?= [40]

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

tests = $testGroupGenerator