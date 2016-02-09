{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

-- | Some helpers for non-empty list that don't exist.
-- TODO: consider a PR for Aeson for NEL, but maybe there
-- is a good reason they don't have it.
module Scrabble.NonEmpty (Scrabble.NonEmpty.foldl) where

import Data.Aeson
import Data.List.NonEmpty(NonEmpty((:|)), toList, fromList)

instance ToJSON a => ToJSON (NonEmpty a) where
  toJSON = toJSON . toList

instance FromJSON a => FromJSON (NonEmpty a) where
  parseJSON v = fromList <$> parseJSON v

-- | fold, but get the initial state by applying a function
-- to the head of the nel
foldl :: (b -> a -> b) -> (a -> b) -> NonEmpty a -> b
foldl fbab fab (a:|rest) = Prelude.foldl fbab (fab a) rest
