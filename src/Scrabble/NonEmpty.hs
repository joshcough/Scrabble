{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

-- | Some helpers for non-empty list that don't exist.
-- TODO: consider a PR for Aeson for NEL, but maybe there
-- is a good reason they don't have it.
module Scrabble.NonEmpty (Scrabble.NonEmpty.foldl) where

import Data.Aeson
import Data.List.NonEmpty(NonEmpty((:|)))
import qualified Data.List.NonEmpty as NE

deriving instance ToJSON a => ToJSON (NonEmpty a)
deriving instance FromJSON a => FromJSON (NonEmpty a)

-- | fold, but get the initial state by applying a function
-- to the head of the nel
foldl :: (b -> a -> b) -> (a -> b) -> NonEmpty a -> b
foldl fbab fab (a:|rest) = Prelude.foldl fbab (fab a) rest
