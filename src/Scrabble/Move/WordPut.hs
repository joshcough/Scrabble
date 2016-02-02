{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

-- | Code to represent putting tiles on the board.
module Scrabble.Move.WordPut where

import Data.Aeson (ToJSON, FromJSON)
import qualified Data.Maybe as Maybe
import GHC.Generics
import Prelude hiding (Word)
import Scrabble.Bag
import Scrabble.Board
import Scrabble.Dictionary
import Scrabble.Position

-- | Represents a single tile being put on the board
data TilePut =
   LetterTilePut Tile   Point -- ^ This tile has a letter on it
 | BlankTilePut  Letter Point -- ^ The blank tile was played, and the letter that the player intends use.
 deriving (Eq, Generic, ToJSON, FromJSON)

-- TODO: when are these shown? show the position be shown too?
instance Show TilePut where
  show (LetterTilePut t _) = show $ letter t
  show (BlankTilePut  l _) = show l

instance HasLetter TilePut where
  letter (LetterTilePut t _) = letter t
  letter (BlankTilePut  l _) = l

asTile :: TilePut -> Tile
asTile (LetterTilePut t _) = t
asTile (BlankTilePut  l _) = fromLetter l

tilePutPoint :: TilePut -> Point
tilePutPoint (LetterTilePut _ p) = p
tilePutPoint (BlankTilePut  _ p) = p

{- A complete representation of placing a word on the board. -}
data WordPut = WordPut { wordPutTiles :: [TilePut] }
  deriving (Eq, Generic, ToJSON, FromJSON, Show)

-- | Make a WordPut from all the given data
-- some error handling isn't happening here and this code is pretty bad
-- TODO: explain how the string translates to Tiles.
makeWordPut :: String
           ->  Orientation
           ->  Point
           ->  [Char]
           ->  Either String WordPut
makeWordPut w o p blanks = WordPut <$> putTils where

  -- TODO: this code sucks...what is it even doing?
  coords :: [(Int,Int)]
  coords = reverse . fst $ foldl f ([],p) w where
    f (acc,p) c = (p:acc, foldOrientation rightOfP belowP o p)

  putTils :: Either String [TilePut]
  putTils = Maybe.catMaybes <$> (sequence $ zipWith f w (zip coords [0..])) where
    f :: Char -> ((Int,Int),Int) -> Either String (Maybe TilePut)
    f '@' _     = Right Nothing
    f '_' (p,i) = plt (blanks !! i) p
    f  c  (p,_) = plt c p
    -- TODO: why is this function called plt? also, add comment.
    plt :: Char -> Point -> Either String (Maybe TilePut)
    plt c p     = Just <$> (LetterTilePut <$> maybe (err c) Right (tileFromChar c) <*> pure p)
    err c       = Left $ "invalid character: " ++ [c]
