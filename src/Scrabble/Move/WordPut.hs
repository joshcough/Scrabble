{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

-- | Code to represent putting tiles on the board.
module Scrabble.Move.WordPut where

import Data.Aeson           (ToJSON, FromJSON, toJSON, parseJSON, withArray)
import Data.Aeson.Types     (Parser)
import Data.Map             (Map)
import GHC.Generics
import Prelude hiding       (Word)
import Scrabble.Bag
import Scrabble.Board.Board

import qualified Data.Maybe  as Maybe
import qualified Data.Map    as Map
import qualified Data.Vector as V

-- | Represents a single tile being put on the board
data TilePut =
   LetterTilePut Tile   Point -- ^ This tile has a letter on it
 | BlankTilePut  Letter Point -- ^ The blank tile was played, and the letter that the player intends use.
 deriving (Eq, Generic)

-- TODO: when are these shown? show the position be shown too?
instance Show TilePut where
  show (LetterTilePut t _) = show $ letter t
  show (BlankTilePut  l _) = show l

instance HasLetter TilePut where
  letter (LetterTilePut t _) = letter t
  letter (BlankTilePut  l _) = l

instance ToJSON TilePut where
  toJSON (LetterTilePut t p) = toJSON (p, letter t, score t)
  toJSON (BlankTilePut  l p) = toJSON (p, l, 0 :: Int)

instance FromJSON TilePut where
  parseJSON = withArray "TilePut" $ \arr ->
    let [ps,ls,ss] = V.toList arr
    in do point  <- parseJSON ps :: Parser Point
          letter <- parseJSON ls :: Parser Letter
          score  <- parseJSON ss :: Parser Int
          return $  if score == 0
                    then BlankTilePut letter point
                    else LetterTilePut (fromLetter letter) point

asTile :: TilePut -> Tile
asTile (LetterTilePut t _) = t
asTile (BlankTilePut  l _) = fromLetter l

asBlankTile :: TilePut -> Tile
asBlankTile (LetterTilePut t _) = t
asBlankTile (BlankTilePut _ _)  = fromLetter Blank

tilePutPoint :: TilePut -> Point
tilePutPoint (LetterTilePut _ p) = p
tilePutPoint (BlankTilePut  _ p) = p

{- A complete representation of placing a word on the board. -}
data WordPut = WordPut { wordPutTiles :: [TilePut] }
  deriving (Eq, Generic, ToJSON, FromJSON, Show)

-- | Make a WordPut from all the given data
-- Letters A-Z are mapped to their natural points
--   if there is a tile already on the board at that point
--   then this will fail when it is put on the board
-- '@' means use the letter already on the board at the point
--   if there isn't a tile on the board at that point
--   then this will fail when it is put on the board
-- '_' means use the blank tile, but this requires that
--   the player specifies which letter to use for the blank
--   this is what the blanks argument is for. each _ is mapped
--   to a letter in the blanks list, in the order that makes sense.
--   for example if given, "HE_L_" and ['L','O'], then
--   the first blank is mapped to L, and the second to O.
-- TODO: this is basically the same as putManyWords in Game... so what gives?
makeWordPut :: String
           ->  Orientation
           ->  Point
           ->  [Char]
           ->  Either String WordPut
makeWordPut w o p blanks = WordPut <$> putTils where
  -- get the coordinates for each letter
  points :: [Point]
  points = reverse . fst $ foldl f ([],p) w where
    f (acc,p) _ = (p:acc, foldOrientation rightOfP belowP o p)

  -- map each _ (blank tile) to the letter it will represent
  blankIndices :: Map Point Int
  blankIndices = Map.fromList $
    zip (fst <$> filter (('_' ==) . snd) (zip points w)) [0..]

  putTils :: Either String [TilePut]
  putTils = Maybe.catMaybes <$> (sequence $ zipWith f w points) where
    f :: Char -> Point -> Either String (Maybe TilePut)
    f '@' _ = Right Nothing
    f '_' p = Right $ do
      ix <- Map.lookup p blankIndices
      ls <- wordFromString blanks
      return $ BlankTilePut (ls!!ix) p
    f  c  p = Just <$> (LetterTilePut <$> tileFromCharEither c <*> pure p)
