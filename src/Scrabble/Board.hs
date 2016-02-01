{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

-- | Board representation
module Scrabble.Board (
  Board
 ,Bonus(..)
 ,Orientation(..)
 ,Square(..)
 ,(!)
 ,catOrientation
 ,centerPosition
 ,debugSquare
 ,elemAt
 ,emptySquare
 ,isBoardEmpty
 ,neighbors
 ,newBoard
 ,printBoard
 ,putTiles
 ,showBoard
 ,taken
 ,toWord
 ,wordsAtPoints
) where

import Data.Aeson (ToJSON, FromJSON, toJSON, parseJSON)
import qualified Data.Aeson as J
import Data.Array (Array, listArray)
import qualified Data.Array as A
import Data.List (intercalate)
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector as V
import GHC.Generics
import Scrabble.Bag
import Scrabble.Dictionary
import Scrabble.Position

data Bonus  = W3 | W2 | L3 | L2 | Star | NoBonus
  deriving (Eq, Ord, Generic, ToJSON, FromJSON)

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
} deriving (Eq, Ord, Generic, ToJSON, FromJSON)

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

data Orientation = Horizontal | Vertical
  deriving (Eq, Ord, Generic, ToJSON, FromJSON, Show)

catOrientation :: a -> a -> Orientation -> a
catOrientation l _ Horizontal = l
catOrientation _ r Vertical   = r

data Board = Board {
  contents :: (Array (Int, Int) Square)
} deriving (Eq, Ord, Generic)

instance ToJSON Board where
  toJSON (Board b) = toJSON . filter f $ A.assocs b where
    f (_,s) = Maybe.isJust $ tile s

instance FromJSON Board where
  parseJSON = J.withArray "Board" $ \arr ->
    Board . (newBoard //) <$> mapM parseJSON (V.toList arr)

type Row = Array Int Square
type Col = Array Int Square

boardBounds :: ((Int, Int), (Int, Int))
boardBounds = ((0,0), (14,14))

infixl 9  !, //
(!) :: Board -> (Int, Int) -> Square
(Board b) ! p = b A.! p
(//) :: Board -> [((Int, Int), Square)] -> Array (Int, Int) Square
(Board b) // p = b A.// p
elems :: Board -> [Square]
elems (Board b) = A.elems b

newBoard  :: Board
newBoard = Board $ listArray boardBounds (f <$> boardBonuses) where
  f (p,b) = Square Nothing b p

putTiles  :: Board -> [(Point,Tile)] -> Board
putTiles b pts = Board $ b // (f <$> pts) where
  f (p,t) = (p, (b ! p) { tile = Just t })

-- | 'show' for Scrabble boards. The Bool is to show square bonuses, or not.
showBoard :: Bool -> Board -> String
showBoard printBonuses board = top ++ showRows ++ bottom where
  showRows      = intercalate "\n" (showRow <$> (A.elems $ rows board)) ++ "\n"
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
boardToList (Board b) = A.elems b

isBoardEmpty :: Board -> Bool
isBoardEmpty = all emptySquare . boardToList

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

-- |
wordsAtPoints ::
    [Point]      -- ^ all the points a player placed tiles in this turn
  -> Board       -- ^ the board (with those tiles on it)
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

-- | all the empty positions on the board
--   that are have a neighbor with a tile in them
emptyConnectedPositions :: Board -> [Square]
emptyConnectedPositions b =
  filter legalSquare $ elems b where
    -- is it legal to put a tile in this square?
    -- if its filled, it's obviously not legal
    legalSquare s | taken s = False
    -- otherwise, if it has any filled neighbors, it's ok.
    legalSquare s = or $ taken <$> neighbors b (squarePos s)

-- | all the neighbors of a particular square (left,right,up,down)
neighbors :: Board -> Point -> [Square]
neighbors b p = [b ! (x,y) | (x,y) <- neighborsP p, inbounds p]

-- | returns True if a point is within board bounds
inbounds :: Point -> Bool
inbounds (x,y) = f x && f y where f i = i >= 0 && i <= 14

printBoard :: Bool -> Board -> IO ()
printBoard showBonuses = putStrLn . showBoard showBonuses