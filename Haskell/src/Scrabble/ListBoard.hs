{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Scrabble.ListBoard where

import Data.Char (toUpper)
import Data.Either (isRight)
import Data.List (foldl', intercalate)
import Data.Maybe (Maybe)
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import Debug.Trace
import qualified Data.Set as Set
import Prelude hiding (Word)
import Scrabble.Bag
import Scrabble.Board
import Scrabble.Matrix
import Scrabble.Types

type ListBoard  = ListMatrix Square

instance Board ListMatrix where
  putTile   = putTileOnListBoard
  putWord   = putWordOnListBoard
  showBoard = showListBoard
  getWordAt = listBoardGetWordAt
  newBoard  = newListBoard
  runChecks = runChecksListBoard

showListBoard :: Bool -> ListBoard -> String
showListBoard printBonuses (LM board) = top ++ showRows ++ bottom where
  showRows      = intercalate "\n" (fmap showRow board) ++ "\n"
  showRow     r = "|" ++ concat (fmap showSquare' r)
  showSquare' s = showSquare printBonuses s ++ "|"
  top           = line '_'
  bottom        = line '-'
  line        c = replicate 46 c ++ "\n"

printListBoard :: Bool -> ListBoard -> IO ()
printListBoard b = putStrLn . showListBoard b

{- create a new, empty Scrabble board -}
newListBoard :: ListBoard
newListBoard = fmap f (LM boardBonuses) where
  f (pos,b) = Square Nothing b pos

{- get the word at the giving position, by orientation, if one exists -}
listBoardGetWordAt :: Pos p => ListBoard -> p -> Orientation -> Maybe [Square]
listBoardGetWordAt b p o = tile <$> here >>= f where
  here       = elemAt b p
  beforeHere = reverse . taker . reverse $ beforeByOrientation o b p
  afterHere  = taker $ afterByOrientation o b p
  taker      = takeWhile taken
  word       = beforeHere ++ maybe [] (:[]) here ++ afterHere
  f _        = if length word > 1 then Just word else Nothing

{- place a single tile, without worrying about scoring -}
putTileOnListBoard :: Pos p => ListBoard -> p -> Tile -> ListBoard
putTileOnListBoard (LM b) p t = LM (mapNth newRow (y p) b) where
  newRow = mapNth (\(Square _ b p) -> Square (Just t) b p) (x p)
  mapNth :: (a -> a) -> Int -> [a] -> [a]
  mapNth f i as = xs ++ [f $ head ys] ++ drop 1 ys where (xs,ys) = splitAt i as

{- lay tiles down on the board. calculate the score of the move -}
putWordOnListBoard :: ListBoard -> PutWord -> Either String (ListBoard, Score)
putWordOnListBoard b pw = do
  squares <- squaresPlayedThisTurn
  let b' = nextBoard $ zip squares (tiles pw)
  runChecksListBoard (zipSquaresAndTiles squares) b b'
  return (b', turnScore squares b') where

  turnScore :: [Square] -> ListBoard -> Int
  turnScore sqrs brd = calculateScore sqrs brd

  squaresPlayedThisTurn :: Either String [Square]
  squaresPlayedThisTurn = traverse f (pos <$> tiles pw) where
    f :: Position -> Either String Square
    f p = maybe (Left $ "out of bounds: " ++ show p) Right $ elemAt b p

  zipSquaresAndTiles :: [Square] -> [(Square,PutTile,Position)]
  zipSquaresAndTiles sqrs = zipWith (\s t -> (s, t, pos t)) sqrs (tiles pw)

  nextBoard :: [(Square,PutTile)] -> ListBoard
  nextBoard sqrs = foldl f b sqrs where
    f acc ((Square _ _ p), pt) = putTile acc p (g pt) where
      g (PutLetterTile t _) = t
      g (PutBlankTile  l _) = mkTile l

{- checks if everything in a move is good -}
runChecksListBoard ::
  [(Square,PutTile,Position)] -> -- all the letters put down this turn
  ListBoard -> -- old board
  ListBoard -> -- new board
  Either String ()
runChecksListBoard squaresAndTiles b b' = checkPuts >> return () where
  firstPos = pos . (\(_,_,p) -> p) $ head squaresAndTiles

  {- TODO: I think check puts should happen before this,
           when the PutTiles are created.
   -}
  {- checkPuts returns true if
       * all the letters were put down on empty squares
       * a tile was placed in all the empty tiles in the word
   -}
  checkPuts :: Either String ()
  checkPuts = traverse checkPut squaresAndTiles >> return () where
    -- make sure we haven't put something in a tile thats already taken
    checkPut :: (Square, PutTile, Position) -> Either String ()
    checkPut ((Square (Just t) _ _), _, p) =
      Left $ "square taken: " ++ show t ++ ", " ++ show p
    checkPut _ = Right ()

{- Calculate the score for ALL words in a turn -}
calculateScore ::
  [Square]  -> -- all the squares a player placed tiles in this turn
  ListBoard -> -- the board (with those tiles on it)
  Score
calculateScore squaresPlayedThisTurn nextBoard = turnScore where
  wordsPlayedThisTurn :: Set [Square]
  wordsPlayedThisTurn = Set.fromList . concat $ f <$> squaresPlayedThisTurn where
    f s = getWordsTouchingSquare s nextBoard
  squaresSet :: Set Square
  squaresSet = Set.fromList squaresPlayedThisTurn
  turnScore :: Score
  turnScore = foldl f 0 (Set.toList wordsPlayedThisTurn) where
    f acc w = scoreWord w squaresSet + acc

{- put some words on a brand new board -}
quickPut :: [(String, Orientation, (Int, Int))] -> (ListBoard,[Score])
quickPut words = either error id  $ quickPut' words newBoard

{- put some words onto an existing board -}
quickPut' :: [(String, Orientation, (Int, Int))] ->
             ListBoard ->
             Either String (ListBoard,[Score])
quickPut' words b = go (b,[]) putWords where

  {- TODO: this is pretty awful
     I think EitherT over State could clean it up,
     but not sure if i want to do that.
  -}
  go :: (ListBoard, [Score]) -> [PutWord] -> Either String (ListBoard, [Score])
  go (b,ss) pws = foldl f (Right (b,ss)) pws where
    f acc pw = do
      (b,scores) <- acc
      (b',score) <- putWord b pw
      return (b',score:scores)

  putWords :: [PutWord]
  putWords =  (\(s,o,p) -> toPutWord s o p) <$> words where
    toPutWord :: String -> Orientation -> (Int, Int) -> PutWord
    toPutWord w o (x,y) = PutWord putTils where
      adder :: (Int, Int) -> (Int, Int)
      adder = catOrientation (\(x,y) -> (x+1,y)) (\(x,y) -> (x,y+1)) o
      coordinates :: [(Int,Int)]
      coordinates = reverse . fst $ foldl f ([],(x,y)) w where
        f (acc,(x,y)) c = ((x,y):acc, adder (x,y))
      putTils :: [PutTile]
      putTils = zipWith f w coordinates where
        f c xy = PutLetterTile (mkTile c) (pos xy)
