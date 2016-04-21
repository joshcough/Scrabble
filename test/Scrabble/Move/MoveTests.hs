{-# LANGUAGE TemplateHaskell #-}

module Scrabble.Move.MoveTests where

import Test.Framework.Providers.HUnit
import Test.Framework.TH
import Test.HUnit
import TestHelpers
import Scrabble.Move.Move
import Scrabble.Board.Board
import Scrabble.Bag


case_create_move =
    let rack = Rack    [ fromLetter O, fromLetter P, fromLetter T ]
        wp   = WordPut [ LetterTilePut (fromLetter T) (7,7)
                       , LetterTilePut (fromLetter O) (7,8)
                       , LetterTilePut (fromLetter P) (7,9)
                       ]
        expected = do
            (board',score) <- wordPut standardValidation newBoard wp dict
            return $ Move wp score (Rack []) board'

    in createMove newBoard rack wp dict @?= expected


case_create_move_single_blank =
    let rack = Rack    [ fromLetter O, fromLetter P, fromLetter Blank]
        wp   = WordPut [ BlankTilePut T (7,7)
                       , LetterTilePut (fromLetter O) (7,8)
                       , LetterTilePut (fromLetter P) (7,9)
                       ]
        expected = do
            (board',score) <- wordPut standardValidation newBoard wp dict
            return $ Move wp score (Rack []) board'

    in createMove newBoard rack wp dict @?= expected


case_create_move_many_blanks =
    let rack = Rack    [ fromLetter Blank, fromLetter O, fromLetter P,  fromLetter Blank]
        wp   = WordPut [ BlankTilePut T (7,7)
                       , LetterTilePut (fromLetter O) (7,8)
                       , LetterTilePut (fromLetter P) (7,9)
                       ]
        expected = do
            (board',score) <- wordPut standardValidation newBoard wp dict
            return $ Move wp score (Rack [fromLetter Blank]) board'

    in createMove newBoard rack wp dict @?= expected


case_create_move_all_blanks =
    let rack = Rack    [ fromLetter Blank, fromLetter O, fromLetter P,  fromLetter Blank]
        wp   = WordPut [ BlankTilePut T (7,7)
                       , LetterTilePut (fromLetter O) (7,8)
                       , LetterTilePut (fromLetter P) (7,9)
                       , BlankTilePut S (7,10)
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