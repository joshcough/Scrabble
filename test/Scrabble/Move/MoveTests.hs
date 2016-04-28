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
    let rack       = Rack $ map fromLetter [O, P, T]
        (Right wp) = makeWordPut "TOP" Vertical (7,7) []
        expected = do
            (board',score) <- wordPut standardValidation newBoard wp dict
            return $ Move wp score (Rack []) board'

    in createMove newBoard rack wp dict @?= expected


case_create_move_single_blank =
    let rack       = Rack $ map fromLetter [O, P, Blank]
        (Right wp) = makeWordPut "_OP" Vertical (7,7) ['T']
        expected = do
            (board',score) <- wordPut standardValidation newBoard wp dict
            return $ Move wp score (Rack []) board'

    in createMove newBoard rack wp dict @?= expected


case_create_move_many_blanks =
    let rack       = Rack $ map fromLetter [ Blank, O, P, Blank]
        (Right wp) = makeWordPut "_OP" Vertical (7,7) ['T']
        expected = do
            (board',score) <- wordPut standardValidation newBoard wp dict
            return $ Move wp score (Rack [fromLetter Blank]) board'

    in createMove newBoard rack wp dict @?= expected


case_create_move_all_blanks =
    let rack       = Rack $ map fromLetter [Blank, O, P, Blank]
        (Right wp) = makeWordPut "_OP_" Vertical (7,7) ['T', 'S']
        expected = do
            (board',score) <- wordPut standardValidation newBoard wp dict
            return $ Move wp score (Rack []) board'

    in createMove newBoard rack wp dict @?= expected

-- this is the case that prompted the algorithm change
case_uses_blank_not_equivalent_tile =
    let rack       = Rack $ map fromLetter [Blank, P, O, P, S]
        (Right wp) = makeWordPut "POP_" Vertical (7,7) ['S']
        expected = do
            (board',score) <- wordPut standardValidation newBoard wp dict
            return $ Move wp score (Rack [fromLetter S]) board'

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