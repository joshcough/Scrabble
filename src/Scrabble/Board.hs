{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Board representation
module Scrabble.Board where

import qualified Data.Maybe as Maybe
import Scrabble.Bag
import Scrabble.Dictionary
import Scrabble.Matrix
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
  squarePos :: Position
} deriving (Eq,Ord)

instance HasPosition Square where
  pos (Square _ _ p) = p

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

class Matrix b => Board b where
  newBoard  :: b Square
  putTile   :: Pos p => b Square -> p -> Tile -> b Square
  {- 'show' for Scrabble boards.
     The Bool is to display square bonuses, or not.
   -}
  showBoard :: b Square -> Bool -> String

boardToList :: (Board b, Foldable b) => b Square -> [Square]
boardToList = foldr (:) []

emptyBoard :: (Board b, Foldable b) => b Square -> Bool
emptyBoard = all emptySquare . boardToList

getWordsAt :: (Vec (Row b), Board b, Pos p) =>
              b Square ->
              p        ->
              (Maybe [Square], Maybe [Square])
getWordsAt b p = (getWordAt b p Horizontal, getWordAt b p Vertical)

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

-- Yes, technically you can have a board that isn't 15x15.
-- But let's consider that a programming error.
-- If the center tile (7,7) is missing, just error out.
centerPosition :: Position
centerPosition = (Position 7 7)

{- all the tiles 'before' a position in a matrix,
   vertically or horizontally -}
beforeByOrientation :: (Pos p, Matrix m) =>
                       Orientation -> m a -> p -> Row m a
beforeByOrientation = catOrientation leftOf above
afterByOrientation :: (Pos p, Matrix m) =>
                      Orientation -> m a -> p -> Row m a
{- all the tiles 'after' a position in a matrix,
   vertically or horizontally -}
afterByOrientation = catOrientation rightOf below

getWordsTouchingSquare :: (Foldable b, Board b, Vec (Row b)) =>
                          Square -> b Square -> [[Square]]
getWordsTouchingSquare s b = Maybe.catMaybes [mh,mv] where
  (mh,mv) = getWordsAt b (pos s)

{-TODO: if i'm not lazy, i wouldn't have to use vecList where
   get the word at the giving position, by orientation,
   if one exists -}
getWordAt :: (Vec (Row b), Board b, Pos p) =>
             b Square -> p -> Orientation -> Maybe [Square]
getWordAt b p o = tile <$> here >>= f where
  here       = elemAt b p
  beforeHere = reverse . taker . reverse . vecList $
                 beforeByOrientation o b p
  afterHere  = taker . vecList $ afterByOrientation o b p
  taker      = takeWhile taken
  word       = beforeHere ++ maybe [] (:[]) here ++ afterHere
  -- no scrabble word can be of length 1.
  f _        = if length word > 1 then Just word else Nothing

-- all the empty positions on the board
-- that are have a neighbor with a tile in them
emptyConnectedPositions :: (Foldable b, Board b) =>
                           b Square -> [Square]
emptyConnectedPositions b =
  filter legalSquare $ foldr (:) [] b where
    -- is it legal to put a tile in this square?
    -- if its filled, it's obviously not legal
    legalSquare s | taken s = False
    -- otherwise, if it has any filled neighbors, it's ok.
    legalSquare s = or $ taken <$> neighbors b (pos s)
