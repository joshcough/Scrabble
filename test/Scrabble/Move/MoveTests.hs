{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Scrabble.Move.MoveTests where

import Test.Framework.Providers.HUnit
import Test.Framework.TH
import Test.HUnit
import TestHelpers
import Scrabble.Move.Move
import Scrabble.Board.Board
import Scrabble.Bag


case_create_move =
    let rack = Rack    [ Tile O 3, Tile P 5, Tile T 3 ]
        wp   = WordPut [ LetterTilePut (Tile T 3) (7,7)
                       , LetterTilePut (Tile O 3) (7,8)
                       , LetterTilePut (Tile P 5) (7,9)
                       ]
        expected = do
            (board',score) <- wordPut standardValidation newBoard wp dict
            return $ Move wp score (Rack []) board'

    in createMove newBoard rack wp dict @?= expected

case_create_move_with_blank =
    let rack = Rack    [ Tile O 3, Tile P 5, Tile Blank 0]
        wp   = WordPut [ BlankTilePut T (7,7)
                       , LetterTilePut (Tile O 3) (7,8)
                       , LetterTilePut (Tile P 5) (7,9)
                       ]
        expected = do
            (board',score) <- wordPut standardValidation newBoard wp dict
            return $ Move wp score (Rack []) board'

    in createMove newBoard rack wp dict @?= expected


-- Orphan, but needed for hunit
instance Show Move where
    show (Move wp ps r b) =
        unwords [ "Move"
                , show wp
                , show ps
                , show r
                , showBoard True b
                ]


tests = $testGroupGenerator