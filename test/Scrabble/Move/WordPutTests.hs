{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Scrabble.Move.WordPutTests where

import Scrabble.Board.Board
import Scrabble.Dictionary
import Scrabble.Move.WordPut
import Scrabble.Tile
import Test.Framework.Providers.HUnit
import Test.Framework.TH
import Test.HUnit

case_use_blank_H      = quickPut_H "HE_LO" [L]     @?= (at (7,7) horz [l H, l E, b L, l L, l O])
case_use_two_blanks_H = quickPut_H "HE_L_" [L, O]  @?= (at (7,7) horz [l H, l E, b L, l L, b O])
case_use_all_blanks_H = quickPut_H "___"   [W,A,T] @?= (at (7,7) horz [b W, b A, b T])

case_use_blank_V      = quickPut_V "HE_LO" [L]     @?= (at (7,7) vert [l H, l E, b L, l L, l O])
case_use_two_blanks_V = quickPut_V "HE_L_" [L, O]  @?= (at (7,7) vert [l H, l E, b L, l L, b O])
case_use_all_blanks_V = quickPut_V "___"   [W,A,T] @?= (at (7,7) vert [b W, b A, b T])

horz = Horizontal
vert = Vertical
rw   = Right . WordPut
l l' = LetterTilePut $ fromLetter l'
b    = BlankTilePut

quickPut_H :: String -> [Letter] -> Either String WordPut
quickPut_H = quickPut horz

quickPut_V :: String -> [Letter] -> Either String WordPut
quickPut_V = quickPut vert

quickPut :: Orientation -> String -> [Letter] -> Either String WordPut
quickPut o w bs = makeWordPut w o (7,7) $ toChar <$> bs

at :: Point -> Orientation -> [Point -> TilePut] -> Either String WordPut
at p o fs = Right . WordPut $ zipWith ($) fs (p : allAfterByOrientationP o p)

tests = $testGroupGenerator
