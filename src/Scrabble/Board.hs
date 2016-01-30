{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Board representation
module Scrabble.Board (
  Board
 ,Bonus(..)
 ,Orientation(..)
 ,Square(..)
 ,above
 ,below
 ,beforeByOrientation
 ,catOrientation
 ,centerPosition
 ,col
 ,cols
 ,debugSquare
 ,elemAt
 ,emptyBoard
 ,emptySquare
 ,getWordAt
 ,getWordsAt
 ,getWordsTouchingSquare
 ,getWordsTouchingPoint
 ,leftOf
 ,neighbors
 ,newBoard
 ,printBoard
 ,putTile
 ,rightOf
 ,row
 ,rows
 ,showBoard
 ,taken
 ,toWord
) where

import Data.Array
import Data.List (intercalate)
import qualified Data.Maybe as Maybe
import Scrabble.Bag
import Scrabble.Dictionary
import Scrabble.Position

data Bonus  = W3 | W2 | L3 | L2 | Star | NoBonus deriving (Eq,Ord)

instance Show Bonus where
  show W3      = "3W"
  show W2      = "2W"
  show L3      = "3L"
  show L2      = "2L"
  show Star    = " *"
  show NoBonus = "  "

data Square = Square {
  tile      :: Maybe Tile,
  bonus     :: Bonus,
  squarePos :: Point
} deriving (Eq,Ord)


instance Show Square where
  show = showSquare True

emptySquare :: Square -> Bool
emptySquare (Square Nothing _ _) = True
emptySquare _                    = False

taken :: Square -> Bool
taken = not . emptySquare

showSquare :: Bool -> Square -> String
showSquare printBonus (Square mt b _) =
  maybe (if printBonus then show b else "  ") (\t -> ' ' : show (letter t)) mt

debugSquare :: Square -> String
debugSquare (Square mt b p) = concat
  ["Square {tile:", show mt, ", bonus:", show b, ", pos:", show p]

debugSquareList :: [Square] -> String
debugSquareList ss = show $ debugSquare <$> ss

toWord :: [Square] -> [Letter]
toWord sqrs = letter <$> Maybe.catMaybes (tile <$> sqrs)

data Orientation = Horizontal | Vertical deriving (Eq, Show)

catOrientation :: a -> a -> Orientation -> a
catOrientation l _ Horizontal = l
catOrientation _ r Vertical   = r

type Board = Array (Int, Int) Square
type Row = Array Int Square
type Col = Array Int Square

newBoard  :: Board
newBoard = listArray ((0,0), (14,14)) (f <$> boardBonuses) where
  f (p,b) = Square Nothing b p

putTile   :: Board -> Point -> Tile -> Board
putTile b p t = b // [(p, (b ! p) { tile = Just t })]

-- | 'show' for Scrabble boards. The Bool is to show square bonuses, or not.
showBoard :: Board -> Bool  -> String
showBoard board printBonuses = top ++ showRows ++ bottom where
  showRows      = intercalate "\n" (showRow <$> (elems $ rows board)) ++ "\n"
  showRow     r = "|" ++ concat (fmap showSquare' r)
  showSquare' s = showSquare printBonuses s ++ "|"
  top           = line '_'
  bottom        = line '-'
  line        c = replicate 46 c ++ "\n"

-- | TODO: this Maybe sucks
elemAt  :: Board -> Point -> Maybe Square
elemAt b p = if inbounds p then Just (b ! p) else Nothing
-- | get the row at the given y
row     :: Board -> Int -> Row
row b y = listArray (0,14) [b ! (x, y) | x <- [0..14]]
-- | get the column at the given x
col     :: Board -> Int -> Col
col b x = listArray (0,14) [b ! (x, y) | y <- [0..14]]
-- | get all the rows in the board
rows    :: Board -> Array Int Row
rows b = listArray (0,14) [row b y | y <- [0..14]]
-- | get all the columns in the board
cols    :: Board -> Array Int Col
cols b = listArray (0,14) [col b x | x <- [0..14]]
-- | get all the squares in a column above a point
above   :: Board -> Point -> Row
above b (x,y) = listArray (0,y-1) [b ! (x, y) | y <- [0..y-1]]
-- | get all the squares in a column below a point
below   :: Board -> Point -> Row
below b (x,y) = listArray (0,13-y) [b ! (x, y) | y <- [y+1..14]]
-- | get all the squares in a row left of a point
leftOf  :: Board -> Point -> Col
leftOf b (x,y) = listArray (0,x-1) [b ! (x, y) | x <- [0..x-1]]
-- | get all the squares in a row right of a point
rightOf :: Board -> Point -> Col
rightOf b (x,y) = listArray (0,13-x) [b ! (x, y) | x <- [x+1..14]]

boardToList :: Board -> [Square]
boardToList = foldr (:) []

emptyBoard :: Board -> Bool
emptyBoard = all emptySquare . boardToList

getWordsAt :: Board -> Point -> (Maybe [Square], Maybe [Square])
getWordsAt b p = (getWordAt b p Horizontal, getWordAt b p Vertical)

{- a simple representation of an empty Scrabble board -}
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

{- all the tiles 'before' a position in a matrix,
   vertically or horizontally -}
beforeByOrientation :: Orientation -> Board -> Point -> Row
beforeByOrientation = catOrientation leftOf above
afterByOrientation :: Orientation -> Board -> Point -> Row
{- all the tiles 'after' a position in a matrix,
   vertically or horizontally -}
afterByOrientation = catOrientation rightOf below

getWordsTouchingPoint :: Point -> Board -> [[Square]]
getWordsTouchingPoint p b = Maybe.catMaybes [mh,mv] where
  (mh,mv) = getWordsAt b p

getWordsTouchingSquare :: Square -> Board -> [[Square]]
getWordsTouchingSquare s = getWordsTouchingPoint (squarePos s)

-- | get the word at the giving position, by orientation, if one exists
getWordAt :: Board -> Point -> Orientation -> Maybe [Square]
getWordAt b p o = tile (b ! p) >>= f where
  beforeHere = reverse . taker . reverse . elems $ beforeByOrientation o b p
  afterHere  = taker . elems $ afterByOrientation o b p
  taker      = takeWhile taken
  word       = beforeHere ++ [b ! p] ++ afterHere
  -- no scrabble word can be of length 1.
  f _        = if length word > 1 then Just word else Nothing

-- | all the empty positions on the board
--   that are have a neighbor with a tile in them
emptyConnectedPositions :: Board -> [Square]
emptyConnectedPositions b =
  filter legalSquare $ foldr (:) [] b where
    -- is it legal to put a tile in this square?
    -- if its filled, it's obviously not legal
    legalSquare s | taken s = False
    -- otherwise, if it has any filled neighbors, it's ok.
    legalSquare s = or $ taken <$> neighbors b (squarePos s)

-- | all the neighbors of a particular square (left,right,up,down)
neighbors :: Board -> Point -> [Square]
neighbors b p = [b ! (x,y) | (x,y) <- neighborsP p, inbounds p]

-- | returns True if a point is in the space ((0,0),(14,14))
inbounds :: Point -> Bool
inbounds (x,y) = f x && f y where f i = i >= 0 && i <= 14

printBoard :: Board -> Bool -> IO ()
printBoard b showBonuses = putStrLn $ showBoard b showBonuses