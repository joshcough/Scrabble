{-# LANGUAGE FlexibleContexts #-}

-- | Some quick testing helper functions for use on the REPL.
-- It is okay for the functions in here to be sloppy and throw errors.
module Scrabble.ReplHelpers where

import Data.Aeson
import Data.Aeson.Types
import Data.List.NonEmpty   (NonEmpty((:|)))
import Scrabble.Bag
import Scrabble.Board.Board
import Scrabble.Dictionary
import Scrabble.Game
import Scrabble.Move.Move
import Scrabble.Search
import System.IO.Unsafe

-- | put a word on new board board and print the board immediately
now :: String -> IO ()
now = printBS . now'

-- | put a word on new board board and return the board and score
now' :: String -> (Board Square, [Score])
now' s = quickPut [(s, Horizontal, (7, 7))]

nowNoValidation :: String -> IO ()
nowNoValidation = printBS . nowNoValidation'

nowNoValidation' :: String -> (Board Square, [Score])
nowNoValidation' s = quickPutNoValidation [(s, Horizontal, (7, 7))]

printBS :: (Board Square,[Score]) -> IO ()
printBS (b, scores) = printBoard True b >> putStrLn (show scores)

quickPut :: [(String, Orientation, Point)] -> (Board Square, [Score])
quickPut = quickPut' standardValidation

-- | put a word on new board board and return the board and score
--   but don't do any validation on the word, so that you can put
--   non-words on the board like "erwaeirawer" for testing other things
quickPutNoValidation :: [(String, Orientation, Point)]
                     -> (Board Square,[Score])
quickPutNoValidation = quickPut' noValidation

{- test putting some words on a brand new board -}
quickPut' ::
     Validator
  -> [(String, Orientation, Point)]
  -> (Board Square, [Score])
quickPut' validate words = unsafePerformIO $ do
  dict <- Scrabble.Dictionary.englishDictionary
  let e = putManyWords validate words newBoard dict
  return $ either (\s -> error $ "quickPut' died with: " ++ s) id e

-- | Search the dictionary with a new random rack
testSearchR :: IO (Rack, [String])
testSearchR = do
  Bag bag <- newShuffledBag
  let rack = take 7 bag
  words   <- testSearch <$> dictionary <*> pure (toChar . letter <$> rack)
  return (Rack rack, words)

showScores :: Game -> IO ()
showScores g = putStrLn . show $ getScores g

-- | test if a board can go round trip 'toJSON <- -> parseJSON'
boardRoundTripJSON :: String -> IO ()
boardRoundTripJSON s = case parse f s of
    Error err -> putStrLn err
    Success b -> printBoard True b
  where f s = parseJSON . toJSON . fst $ now' s

unsafeNewGame :: [(Int -> Player)] -> Game
unsafeNewGame (p:ps) = unsafePerformIO $ newGame (p:|ps)
unsafeNewGame _ = error "Error: Game must have at least one player."

unsafeNewBag :: Bag
unsafeNewBag = unsafePerformIO newBag
