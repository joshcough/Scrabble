-- |
module Scrabble.Move.Move (
  module Scrabble.Move.WordPut
 ,module Scrabble.Move.Validation
 ,Move(..)
 ,createMove
 ,wordPut
) where

import Data.List (delete, foldl')
import Prelude hiding (Word)
import Scrabble.Bag
import Scrabble.Board.Board
import Scrabble.Move.Scoring
import Scrabble.Move.Validation
import Scrabble.Move.WordPut
import Scrabble.Search (Search, ups)

-- |
data Move = Move {
  moveWordPut        :: WordPut -- ^ all the tiles laid down
 ,movePointsScored   :: Points  -- ^ points score in turn
 ,moveRackRemaining  :: Rack    -- ^ not yet refilled
 ,moveBoardAfterMove :: Board   -- ^ the state of the board after the move
} deriving (Eq)

-- |
createMove :: Board   -- ^
          ->  Rack    -- ^
          ->  WordPut -- ^
          ->  Dict    -- ^
          ->  Either String Move
createMove = createMove' standardValidation

-- |
createMove' :: Validator -- ^
           ->  Board     -- ^
           ->  Rack      -- ^
           ->  WordPut   -- ^
           ->  Dict      -- ^
           ->  Either String Move
createMove' validate b (Rack rack) wp dict = if valid then go else errMsg where
  errMsg        = Left "error: rack missing input letters"
  rackLetters   = toString (fmap letter rack)
  valid         = containsAllWithBlanks putLetters rackLetters
  putLetters    = toString (letter <$> wordPutTiles wp)
  go = do (newBoard, score) <- wordPut validate b wp dict
          return $ Move wp score (rackRemainder (Rack rack) wp) newBoard


-- | Like containsAll from Scrabble.Search, but if it encounters a character
--   in s1 not in s2, it deletes a blank and tries again before returning false
containsAllWithBlanks :: String -> Search
containsAllWithBlanks s1 s2 = fst $ foldl' f (True, ups s2) (ups s1) where
  f (b,s) c = if elem c s then (b, delete c s)
              else if elem '_' s then (b, delete '_' s)
              else (False, s)


-- | Similar to containsAllwithBlanks, but it leaves the rack and wordPut contex
--   to simplify the iteration.
rackRemainder :: Rack -> WordPut -> Rack
rackRemainder (Rack r) (WordPut tps) = Rack $ foldl' f r tps
    where
      rackLetters = map letter r

      -- Get the tile associated with a TilePut so you can delete it in f
      toTile :: TilePut -> Tile
      toTile (LetterTilePut t _) = t
      toTile (BlankTilePut _ _)  = fromLetter Blank

      f :: [Tile] -> TilePut -> [Tile]
      f remainingTiles tp =
          if elem (letter tp) rackLetters || elem Blank rackLetters
          then delete (toTile tp) remainingTiles
          else remainingTiles


-- | Attempt to lay tiles on the board.
--   Validate the entire move.
--   Calculate the score of the move.
wordPut :: Validator -- ^ validation algo
       ->  Board     -- ^ the board to put the word on
       ->  WordPut   -- ^ the word being put on the board
       ->  Dict      -- ^ the scrabble dictionary
       ->  Either String (Board, Score)
wordPut validate b wp dict = do
  squares <- squaresPlayedThisTurn
  let b' = nextBoard $ wordPutTiles wp
  validate (wordPutTiles wp) b b' dict
  let s  = calculateTurnScore squares b'
  return (b', s) where
    -- all the squares on the board prior to laying the tiles on them
    squaresPlayedThisTurn :: Either String [Square]
    squaresPlayedThisTurn = traverse f (tilePutPoint <$> wordPutTiles wp) where
      f p = maybe (Left $ "out of bounds: " ++ show p) Right $ elemAt b p
    -- actually put all the tiles on the board
    nextBoard :: [TilePut] -> Board
    nextBoard ts = putTiles b $ f <$> ts where
      f tp = (tilePutPoint tp, asTile tp)