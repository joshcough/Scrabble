{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Scrabble.Board where

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
import Scrabble.Matrix
import Scrabble.Types

data Bonus  = W3 | W2 | L3 | L2 | Star | NoBonus deriving (Eq,Ord)

data Square = Square {
  tile      :: Maybe Tile,
  bonus     :: Bonus,
  squarePos :: Position
} deriving (Eq,Ord)

instance HasPosition Square where
  pos (Square _ _ p) = p

instance Show Square where
  show = showSquare True

class Matrix b => Board b where
  newBoard  :: b Square
  putTile   :: Pos p => b Square -> p -> Tile -> b Square

  {- TODO: I think all of the remaining functions
     in this class can be pulled out, because
     the should be sufficiently generic -
     they really only depend on elemAt.
   -}

  {- Put a word on the boar -}
  putWord   :: b Square ->
               PutWord  ->
               Either String (b Square, Score)

  {- Get a word (if there is one) -}
  getWordAt :: Pos p => b Square    ->
                        p           ->
                        Orientation ->
                        Maybe [Square]

  {- 'show' for Scrabble boards.
     The Bool is to display square bonuses, or not.
   -}
  showBoard :: Bool -> b Square -> String

  {- Determine if it's okay to place these tiles
     on the corresponding squares.
   -}
  runChecks :: Pos p => [(Square, PutTile, p)] ->
                        b Square ->
                        b Square ->
                        Either String ()

printBoard :: Board b => Bool -> b Square -> IO ()
printBoard b = putStrLn . showBoard b

getWordsAt :: (Pos p, Board b) => b Square -> p -> (Maybe [Square], Maybe [Square])
getWordsAt b p = (getWordAt b p Horizontal, getWordAt b p Vertical)

instance Show Bonus where
  show W3 = "3W"
  show W2 = "2W"
  show L3 = "3L"
  show L2 = "2L"
  show Star    = " *"
  show NoBonus = "  "

showSquare :: Bool -> Square -> String
showSquare printBonus (Square mt b p) =
  maybe (if printBonus then show b else "  ") (\t -> [' ', letter t]) mt

debugSquare :: Square -> String
debugSquare (Square mt b p) = concat ["Square {tile:", show mt, ", bonus:", show b, ", pos:", show p]

debugSquareList :: [Square] -> String
debugSquareList ss = show $ debugSquare <$> ss

{- a simple representation of an empty Scrabble board -}
boardBonuses :: [[(Position, Bonus)]]
boardBonuses = indexify [
  [W3,  o,  o, L2,  o,  o,  o, W3,  o,  o,  o, L2,  o,  o, W3],
  [ o, W2,  o,  o,  o, L3,  o,  o,  o, L3,  o,  o,  o, W2,  o],
  [ o,  o, W2,  o,  o,  o, L2,  o, L2,  o,  o,  o, W2,  o,  o],
  [L2,  o,  o, W2,  o,  o,  o, L2,  o,  o,  o, W2,  o,  o, L2],
  [ o,  o,  o,  o, W2,  o,  o,  o,  o,  o, W2,  o,  o,  o,  o],
  [ o, L3,  o,  o,  o, L3,  o,  o,  o, L3,  o,  o,  o, L3,  o],
  [ o,  o, L2,  o,  o,  o, L2,  o, L2,  o,  o,  o, L2,  o,  o],
  [W3,  o,  o, L2,  o,  o,  o, (*), o,  o,  o, L2,  o,  o, W3],
  [ o,  o, L2,  o,  o,  o, L2,  o, L2,  o,  o,  o, L2,  o,  o],
  [ o, L3,  o,  o,  o, L3,  o,  o,  o, L3,  o,  o,  o, L3,  o],
  [ o,  o,  o,  o, W2,  o,  o,  o,  o,  o, W2,  o,  o,  o,  o],
  [L2,  o,  o, W2,  o,  o,  o, L2,  o,  o,  o, W2,  o,  o, L2],
  [ o,  o, W2,  o,  o,  o, L2,  o, L2,  o,  o,  o, W2,  o,  o],
  [ o, W2,  o,  o,  o, L3,  o,  o,  o, L3,  o,  o,  o, W2,  o],
  [W3,  o,  o, L2,  o,  o,  o, W3,  o,  o,  o, L2,  o,  o, W3]]
 where
   o = NoBonus
   (*) = Star
   indexify :: [[a]] -> [[(Position, a)]]
   indexify as  = fmap f (zip [0..] as) where
     f (y,l)    = fmap g (zip [0..] l ) where
       g (x,a)  = (Position x y, a)

{- all the tiles 'before' a position in a matrix, vertically or horizontally -}
beforeByOrientation :: (Pos p, Matrix m) => Orientation -> m a -> p -> Row m a
beforeByOrientation = catOrientation leftOf above
afterByOrientation :: (Pos p, Matrix m) => Orientation -> m a -> p -> Row m a
{- all the tiles 'after' a position in a matrix, vertically or horizontally -}
afterByOrientation = catOrientation rightOf below

taken :: Square -> Bool
taken = Maybe.isJust . tile

getWordsTouchingSquare :: (Foldable b, Board b) => Square -> b Square -> [[Square]]
getWordsTouchingSquare s b = Maybe.catMaybes [mh,mv] where (mh,mv) = getWordsAt b (pos s)

{- calculate the score for a single word -}
scoreWord ::
  [Square]   -> -- one of the words played this turn
  Set Square -> -- the set of squares played in this turn
  Score
scoreWord word playedSquares = base * wordMultiplier where
  -- score all the letters
  base = foldl (\s l -> scoreLetter l playedSquares + s) 0 word

  {- the score for a single letter (including its multiplier) -}
  scoreLetter :: Square -> Set Square -> Score
  scoreLetter s@(Square (Just t) bonus _) playedSquares =
    if Set.member s playedSquares then letterBonus t bonus else score t

  {- determine the word multipliers for this word (based on using any 2W or 3W tiles -}
  wordMultiplier = foldl' f 1 $ filter (\s -> Set.member s playedSquares) word where
    f acc (Square _ W3 _) = acc * 3
    f acc (Square _ W2 _) = acc * 2
    f acc _               = acc * 1

  {- determine the multipliers for a single letter -}
  letterBonus :: Tile -> Bonus -> Score
  letterBonus t L3 = 3 * score t
  letterBonus t L2 = 2 * score t
  letterBonus t _  = score t
