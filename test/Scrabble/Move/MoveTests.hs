{-# LANGUAGE TemplateHaskell #-}

module Scrabble.Move.MoveTests where

import Test.Framework.Providers.HUnit
import Test.Framework.TH
import Test.HUnit
import TestHelpers
import Scrabble.Move.Move
import Scrabble.Board.Board
import Scrabble.Bag


-- | Test the state of the rack after a sucessfull move by specifying the initial
--   letters in the rack, a string describing the move you want to make including
--   blanks, a list of the characters you want to use as your choices for the blanks
--   in the order you selected them, and a list of letters that should represent
--   the expected final state of the rack. If those second two arguments confuse
--   you, just know that they are the 2nd and 4th arguments to @makeWordPut@
--   and infer what they do from the behavior of that function
rackRemainderTest :: [Letter] -> String -> [Char] -> [Letter] -> Assertion
rackRemainderTest initialRackLetters wpString blankChoices expectedLetters =
  let rack       = Rack $ map fromLetter initialRackLetters
      (Right wp) = makeWordPut wpString Vertical (7,7) blankChoices
      expected   = do
        (board', score) <- wordPut standardValidation newBoard wp dict
        return $ Move wp score (Rack $ map fromLetter expectedLetters) board'
  in createMove newBoard rack wp dict @?= expected


case_create_move = rackRemainderTest [O,P,T] "TOP" [] []


case_create_move_single_blank = rackRemainderTest [O,P,Blank] "_OP" ['T'] []


case_create_move_many_blanks = rackRemainderTest [Blank, O, P, Blank] "_OP" ['T'] [Blank]


case_create_move_all_blanks = rackRemainderTest [Blank, O, P, Blank] "_OP_" ['T','S'] []

-- | this makes sure that you can choose letters for your blank that also happen
--   be in the rack, and we still consume the blank tile and not the letter tile
case_uses_blank_not_equivalent_tile = rackRemainderTest [Blank, O, P, P, S] "POP_" ['S'] [S]


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