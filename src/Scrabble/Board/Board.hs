{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

-- | Board representation
module Scrabble.Board.Board
  (
    module Scrabble.Board.Orientation
  , module Scrabble.Board.Point
  , module Scrabble.Board.Square
  , Board
  , (!)
  , afterByOrientation
  , beforeByOrientation
  , centerPosition
  , col
  , cols
  , elemAt
  , getWords
  , isBoardEmpty
  , neighbors
  , newBoard
  , printBoard
  , putTiles
  , row
  , rows
  , showBoard
  , wordsAtPoints
  ) where

import Data.Aeson                 (ToJSON, FromJSON, toJSON, parseJSON, withArray)
import Data.Array                 (Array, listArray, elems)
import Data.Bifunctor             (second)
import Data.List                  (intercalate)
import Data.Set                   (Set)
import GHC.Generics
import Scrabble.Bag
import Scrabble.Board.Orientation
import Scrabble.Board.Point
import Scrabble.Board.Square

import Prelude hiding (Word)

import qualified Data.Array  as A
import qualified Data.Maybe  as Maybe
import qualified Data.Set    as Set
import qualified Data.Vector as V

data Board a = Board (Array Point a)
  deriving (Eq, Ord, Generic)

instance ToJSON (Board Square) where
  toJSON (Board b) =
      toJSON . fmap (second tile) . filter (taken . snd) $ A.assocs b

instance FromJSON (Board Square) where
  parseJSON = withArray "Board" $ \arr ->
    putTiles newBoard <$> mapM parseJSON (V.toList arr)

type Row a = Array Int a
type Col a = Array Int a

boardBounds :: (Point, Point)
boardBounds = ((0,0), (14,14))

infixl 9  !, //

-- |
(!) :: Board a -> Point -> a
(Board b) ! p = b A.! p

-- |
(//) :: Board a -> [(Point, a)] -> Array Point a
(Board b) // p = b A.// p

-- elems :: Board -> [Square]
-- elems (Board b) = A.elems b

newBoard  :: Board Square
newBoard = Board $ listArray boardBounds (f <$> boardBonuses) where
  f (p,b) = Square Nothing b p

putTiles  :: Board Square -> [(Point,Tile)] -> Board Square
putTiles b pts = Board $ b // (f <$> pts) where
  f (p,t) = (p, (b ! p) { tile = Just t })

-- | 'show' for Scrabble boards. The Bool is to show square bonuses, or not.
showBoard :: Bool -> Board Square -> String
showBoard printBonuses board = top ++ showRows ++ bottom where
  showRows      = intercalate "\n" (showRow <$> (A.elems $ rows board)) ++ "\n"
  showRow     r = "|" ++ concat (fmap showSquare' r)
  showSquare' s = showSquare printBonuses s ++ "|"
  top           = line '_'
  bottom        = line '-'
  line        c = replicate 46 c ++ "\n"

-- | TODO: this Maybe sucks
elemAt  :: Board a -> Point -> Maybe a
elemAt b p = if inbounds p then Just (b ! p) else Nothing

zeroTo14 :: [Int]
zeroTo14 = [0..14]

-- | get the row at the given y
row     :: Board a -> Int -> Row a
row b y = listArray (0,14) [b ! (x, y) | x <- zeroTo14]

-- | get the column at the given x
col     :: Board a -> Int -> Col a
col b x = listArray (0,14) [b ! (x, y) | y <- zeroTo14]

-- | get all the rows in the board
rows    :: Board a -> Array Int (Row a)
rows b = listArray (0,14) [row b y | y <- zeroTo14]

-- | get all the columns in the board
cols    :: Board a -> Array Int (Col a)
cols b = listArray (0,14) [col b x | x <- zeroTo14]

-- | get all the squares in a column above a point
above   :: Board a -> Point -> Row a
above b (x,y) = listArray (0,y-1) [b ! (x, y) | y <- [0..y-1]]

-- | get all the squares in a column below a point
below   :: Board a -> Point -> Row a
below b (x,y) = listArray (0,13-y) [b ! (x, y) | y <- [y+1..14]]

-- | get all the squares in a row left of a point
leftOf  :: Board a -> Point -> Col a
leftOf b (x,y) = listArray (0,x-1) [b ! (x, y) | x <- [0..x-1]]

-- | get all the squares in a row right of a point
rightOf :: Board a -> Point -> Col a
rightOf b (x,y) = listArray (0,13-x) [b ! (x, y) | x <- [x+1..14]]

-- |
boardToList :: Board a -> [a]
boardToList (Board b) = A.elems b

-- |
isBoardEmpty :: Board Square -> Bool
isBoardEmpty = all emptySquare . boardToList

-- | a simple representation of an empty Scrabble board
boardBonuses :: [(Point, Bonus)]
boardBonuses = zip [(x,y) | x <- [0..14], y <- [0..14]] (concat [
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
  [W3,  o,  o, L2,  o,  o,  o, W3,  o,  o,  o, L2,  o,  o, W3]])
 where o = NoBonus; (*) = Star

centerPosition :: Point
centerPosition = (7, 7)

-- | all the tiles 'before' a position in a matrix, vertically or horizontally
beforeByOrientation :: Orientation -> Board a -> Point -> Row a
beforeByOrientation = foldOrientation leftOf above
-- | all the tiles 'after' a position in a matrix, vertically or horizontally
afterByOrientation :: Orientation -> Board a -> Point -> Row a
afterByOrientation = foldOrientation rightOf below

-- |
wordsAtPoints ::
    [Point]       -- ^ all the points a player placed tiles in this turn
  -> Board Square -- ^ the board (with those tiles on it)
  -> Set [Square]
wordsAtPoints pts b =
  Set.fromList . concat $ getWordsTouchingPoint <$> pts where
  getWordsTouchingPoint p = Maybe.catMaybes [mh,mv] where
    (mh,mv) = (getWordAt p Horizontal, getWordAt p Vertical)
  -- | get the word at the giving position, by orientation, if one exists
  -- TODO: this is really inefficient.
  getWordAt :: Point -> Orientation -> Maybe [Square]
  getWordAt p o = tile (b ! p) >>= f where
    beforeHere = reverse . taker . reverse . A.elems $ beforeByOrientation o b p
    afterHere  = taker . A.elems $ afterByOrientation o b p
    taker      = takeWhile taken
    word       = beforeHere ++ [b ! p] ++ afterHere
    -- no scrabble word can be of length 1.
    f _        = if length word > 1 then Just word else Nothing

{-
-- | all the empty positions on the board
--   that are have a neighbor with a tile in them
emptyConnectedPositions :: Board Square -> [Square]
emptyConnectedPositions b =
  filter legalSquare $ elems b where
    -- is it legal to put a tile in this square?
    -- if its filled, it's obviously not legal
    legalSquare s | taken s = False
    -- otherwise, if it has any filled neighbors, it's ok.
    legalSquare s = or $ taken <$> neighbors b (squarePos s)
-}

-- | all the neighbors of a particular square (left,right,up,down)
neighbors :: Board a -> Point -> [a]
neighbors b p = [b ! (x,y) | (x,y) <- neighbors4P p, inbounds p]

-- | returns True if a point is within board bounds
inbounds :: Point -> Bool
inbounds (x,y) = f x && f y where f i = i >= 0 && i <= 14

printBoard :: Bool -> Board Square -> IO ()
printBoard showBonuses = putStrLn . showBoard showBonuses

-- | Get all the words on the board, including 1 letter words.
--   It returns the squares containing the tiles.
getWords :: Board Square -> [[Square]]
getWords b = rowWords ++ colWords where
  rowWords = concat $ getWordsInRow <$> rows b
  colWords = concat $ getWordsInCol <$> cols b

  getWordsInCol :: Col Square -> [[Square]]
  getWordsInCol = getWordsInRow

  getWordsInRow :: Row Square -> [[Square]]
  getWordsInRow = getWordsInList . elems

  getWordsInList :: [Square] -> [[Square]]
  getWordsInList [] = []
  getWordsInList squares = case takeWord squares of
    (Just w,  [])   -> [w]
    (Just w,  rest) ->  w : getWordsInList rest
    (Nothing, [])   -> []
    (Nothing, rest) -> getWordsInList rest

  takeWord :: [Square] -> (Maybe [Square], [Square])
  takeWord [] = (Nothing, [])
  takeWord squares = go where
    (w,rest) = span (Maybe.isJust . tile) squares
    go = case length w of
      0 -> takeWord (drop 1 rest)
      n -> (Just w, drop n squares)
