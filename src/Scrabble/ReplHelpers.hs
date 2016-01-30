{-# LANGUAGE FlexibleContexts #-}

-- | Some quick testing helper functions for use on the REPL.
module Scrabble.ReplHelpers where

import Data.Char (toUpper)
import Data.Maybe (fromJust)
import Prelude hiding (Word)
import Scrabble.Bag
import Scrabble.Board
import Scrabble.Dictionary
import Scrabble.Game
import Scrabble.Move.Move
import Scrabble.Search
import System.IO.Unsafe

now :: String -> IO ()
now = printBS . now'

now' :: String -> (Board,[Score])
now' s = quickPut [(s, Horizontal, (7, 7))]

nowNoValidation :: String -> IO ()
nowNoValidation = printBS . nowNoValidation'

nowNoValidation' :: String -> (Board,[Score])
nowNoValidation' s = quickPutNoValidation [(s, Horizontal, (7, 7))]

printBS :: (Board,[Score]) -> IO ()
printBS (b, scores) = printBoard b True >> putStrLn (show scores)

quickPut :: [(String, Orientation, (Int, Int))] -> (Board,[Score])
quickPut = quickPut' standardValidation

quickPutNoValidation :: [(String, Orientation, (Int, Int))] -> (Board,[Score])
quickPutNoValidation = quickPut' noValidation

{- test putting some words on a brand new board -}
quickPut' ::
     Validator
  -> [(String, Orientation, (Int, Int))]
  -> (Board,[Score])
quickPut' validator words = unsafePerformIO $ do
  dict <- Scrabble.Dictionary.dictionary
  let e = putManyWords validator words newBoard dict
  return $ either error id e

{- test putting some words onto an existing board
   this is just a test function and its ok if it bombs -}
putManyWords ::
     Validator
  -> [(String, Orientation, (Int, Int))]
  -> Board
  -> Dict
  -> Either String (Board,[Score])
putManyWords validator words b dict = go (b,[]) wordPuts where
  {- TODO: this is pretty awful
     I think EitherT over State could clean it up,
     but not sure if i want to do that.
     Also, I can't put the type here, again.
  -}
  --go :: (Board, [Score]) -> [WordPut] -> Either String (Board, [Score])
  go (b,ss) pws = foldl f (Right (b,ss)) pws where
    f acc wp = do
      (b,scores) <- acc
      (b',score) <- wordPut validator b wp dict
      return (b',scores++[score])

  wordPuts :: [WordPut]
  wordPuts =  (\(s,o,p) -> toWordPut s o p) <$> words where
    toWordPut :: String -> Orientation -> (Int, Int) -> WordPut
    toWordPut w o (x,y) = WordPut putTils where
      adder :: (Int, Int) -> (Int, Int)
      adder = catOrientation (\(x,y) -> (x+1,y)) (\(x,y) -> (x,y+1)) o
      coordinates :: [(Int,Int)]
      coordinates = reverse . fst $ foldl f ([],(x,y)) w where
        f (acc,(x,y)) c = ((x,y):acc, adder (x,y))
      putTils :: [TilePut]
      putTils = zipWith f w coordinates where
        f c xy = LetterTilePut (fromJust $ tileFromChar (toUpper c)) xy

{- Search the dictionary with a new random rack -}
testSearchR :: IO (Rack, [Word])
testSearchR = do
  bag     <- newShuffledBag
  let rack = take 7 bag
  words   <- testSearch (toString $ letter <$> rack)
  return (rack, fromJust . wordFromString <$> words)

showScores :: Game -> IO ()
showScores g = putStrLn . show $ getScores g
