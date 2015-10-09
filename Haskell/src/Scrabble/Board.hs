{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Scrabble.Board where

import Data.List (any, foldl', intersperse, partition)
import Data.Maybe (Maybe)
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Scrabble.Bag
import Scrabble.Matrix
import Scrabble.Search (dictContainsWord)
import Scrabble.Types

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
showSquare printBonus (Square mt b p) =
  maybe (if printBonus then show b else "  ") (\t -> [' ', letter t]) mt

debugSquare :: Square -> String
debugSquare (Square mt b p) = concat
  ["Square {tile:", show mt, ", bonus:", show b, ", pos:", show p]

debugSquareList :: [Square] -> String
debugSquareList ss = show $ debugSquare <$> ss

toWord :: [Square] -> [Letter]
toWord sqrs = letter <$> Maybe.catMaybes (tile <$> sqrs)

class Matrix b => Board b where
  newBoard  :: b Square
  putTile   :: Pos p => b Square -> p -> Tile -> b Square
  {- 'show' for Scrabble boards.
     The Bool is to display square bonuses, or not.
   -}
  showBoard :: Bool -> b Square -> String

printBoard :: Board b => Bool -> b Square -> IO ()
printBoard b = putStrLn . showBoard b

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
centerPosition = (Position 7 7)
center :: Board b => b Square -> Square
center b = Maybe.fromMaybe
             (error "no center tile")
             (elemAt b centerPosition)

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

{- calculate the score for a single word -}
scoreWord ::
  [Square]   -> -- one of the words played this turn
                -- (and the one we are getting the score for)
  Set Square -> -- the set of squares played in this turn
  Score
scoreWord word playedSquares =
  base * wordMultiplier * centerMultiplier where

  -- score all the letters
  base = foldl (\s l -> scoreLetter l playedSquares + s) 0 word

  {- the score for a single letter (including its multiplier) -}
  scoreLetter :: Square -> Set Square -> Score
  scoreLetter s@(Square (Just t) bonus _) playedSquares =
    if Set.member s playedSquares
    then letterBonus t bonus
    else score t

  {- determine the word multipliers for this word
    (based on using any 2W or 3W tiles -}
  wordMultiplier = foldl' f 1 $ filter g word where
    f acc (Square _ W3 _) = acc * 3
    f acc (Square _ W2 _) = acc * 2
    f acc _               = acc * 1
    g s = Set.member s playedSquares

  {- determine the multipliers for a single letter -}
  letterBonus :: Tile -> Bonus -> Score
  letterBonus t L3 = 3 * score t
  letterBonus t L2 = 2 * score t
  letterBonus t _  = score t

  {- if the center square was played, score*2 -}
  centerMultiplier = if centerSquarePlayed then 2 else 1
  centerSquarePlayed =
    or $ f <$> Set.toList playedSquares where
      f s = pos s == centerPosition


{- lay tiles on the board.
   validate all the things.
   calculate the score of the move. -}
putWord :: (Foldable b, Board b, Vec (Row b))  =>
           b Square ->
           PutWord  ->
           Dict     -> -- the dictionary
           Either String (b Square, Score)
putWord b pw dict = do
  squares <- squaresPlayedThisTurn
  let b' = nextBoard $ zip squares (tiles pw)
  validateMove (zipSquaresAndTiles squares) b b' dict
  return (b', calculateTurnScore squares b') where

  squaresPlayedThisTurn :: Either String [Square]
  squaresPlayedThisTurn = traverse f (pos <$> tiles pw) where
    f :: Position -> Either String Square
    f p = maybe (Left $ "out of bounds: " ++ show p) Right $ elemAt b p

  zipSquaresAndTiles :: [Square] -> [(Square,PutTile,Position)]
  zipSquaresAndTiles sqrs = zipWith f sqrs (tiles pw) where
    f s t = (s, t, pos t)

  --TODO: the compiler won't let me put this type sig here.
  --      even with ScopedTypeVariables.
  --nextBoard :: [(Square,PutTile)] -> b Square
  nextBoard sqrs = foldl f b sqrs where
    f acc ((Square _ _ p), pt) = putTile acc p (asTile pt)

{- Calculate the score for ALL words in a turn -}
calculateTurnScore :: (Foldable b, Board b, Vec (Row b)) =>
  [Square] -> -- all the squares a player placed tiles in this turn
  b Square -> -- the board (with those tiles on it)
  Score
calculateTurnScore sqrs nextBoard = totalScore where
  totalScore = foldl f 0 s + bingoBonus
  s = Set.toList $ wordsPlayedInTurn sqrs nextBoard
  f acc w = scoreWord w (Set.fromList sqrs) + acc
  bingoBonus = if length sqrs == 7 then 50 else 0

wordsPlayedInTurn :: (Foldable b, Board b, Vec (Row b)) =>
  [Square] -> -- all the squares a player placed tiles in this turn
  b Square -> -- the board (with those tiles on it)
  Set [Square]
wordsPlayedInTurn squaresPlayedThisTurn nextBoard =
  Set.fromList . concat $ f <$> squaresPlayedThisTurn where
    f s = getWordsTouchingSquare s nextBoard

-- helper data for packing information about a
-- square played on a particular turn.
data SquareLegality = SquareLegality {
  square     :: Square
 ,available  :: Bool -- is the square not taken
 ,connected  :: Bool -- if touching another tile
 ,centerPlay :: Bool -- if square is the center square
} deriving Show

{- checks if everything in a move is good

		1) At least one tile must be tentative
		2) All tentative tiles must lie on one line
		3) On the first turn, one tile must be located on square START_POSITION
		4) Linear word must be unbroken (including locked tiles)
		5) On every other turn, at least one crossword must be formed
		6) All words formed must be inside the dictionary

-}
validateMove ::
  (Vec (Row b), Foldable b, Board b, Pos p, Show p) =>
  [(Square,PutTile,p)] -> -- all the letters put down this turn
  b Square -> -- old board
  b Square -> -- new board
  Dict     -> -- the dictionary
  Either String ()
validateMove move b b' dict = go where
  go =
    if null move
      then Left "You must place at least one tile!"
    else if emptyB && not centerSquarePlayed
      then Left "Must use center square!"
    else if emptyB && length move <= 1
      then Left "First move must have more than one letter!"
    else if not allConnected
      then Left "Unconnected letters!"
    else if not $ null takenSquares
      then Left squaresTakenError
    else if not $ null badWords
      then Left $ "Bad words: " ++ show badWords
    else Right ()

  -- so we don't have to calculate it twice
  emptyB = emptyBoard b

  legals :: [SquareLegality]
  legals = f <$> squaresPlayedInMove where
    f s = SquareLegality s
      (emptySquare s)
      (or $ taken <$> neighbors b' (pos s))
      (pos s == centerPosition)

  centerSquarePlayed = or $ centerPlay <$> legals

  allConnected :: Bool
  allConnected = and $ connected <$> legals

  squaresPlayedInMove = (\(s,_,_) -> s) <$> move

  takenSquares :: [Square]
  takenSquares = square <$> filter (not . available) legals

  words :: [[Letter]]
  words = toWord <$> (Set.toList $
    wordsPlayedInTurn squaresPlayedInMove b')

  (_, badWords) = partition (dictContainsWord dict) words

  squaresTakenError = "Error, squares taken: " ++
    show (intersperse "," $ debugSquare <$> takenSquares)

-- all the empty positions on the board
-- that are have a neighbor with a tile in them
emptyConnectedPositions :: (Foldable b, Board b) =>
                           b Square -> [Square]
emptyConnectedPositions b =
  filter (legalSquare b) $ foldr (:) [] b where
    -- is it legal to put a tile in this square?
    -- if its filled, it's obviously not legal
    legalSquare b s | taken s = False
    -- otherwise, if it has any filled neighbors, it's ok.
    legalSquare b s = or $ taken <$> neighbors b (pos s)

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
