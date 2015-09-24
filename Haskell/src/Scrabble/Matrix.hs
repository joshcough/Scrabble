{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Scrabble.Matrix
  ( Matrix(..)
  , ListMatrix
  , pattern LM
  , Vec(..)
  ) where

import Data.Functor.Compose (Compose(..))
import Data.Maybe (Maybe,fromMaybe,catMaybes)
import Scrabble.Types (Pos(..))

class Vec m where
  before :: Int -> m a -> m a
  after  :: Int -> m a -> m a

-- | A generic matrix type 'm', whose rows are
-- represented by the associated 'Row'.
class Matrix m where
  type Row m :: * -> *
  elemAt  :: Pos p => (m a) -> p -> Maybe a
  row     :: m a -> Int -> Maybe (Row m a)
  col     :: m a -> Int -> Maybe (Row m a)
  rows    :: m a -> m a
  cols    :: m a -> m a
  above   :: Pos p => m a -> p -> Row m a
  below   :: Pos p => m a -> p -> Row m a
  leftOf  :: Pos p => m a -> p -> Row m a
  rightOf :: Pos p => m a -> p -> Row m a

-- | '[[a]]' is an 'a' "matrix".
type ListMatrix = Compose [] []

-- | A shorthand for getting the list-of-list out of a 'ListMatrix',
-- and putting one back in.
pattern LM a = Compose a

instance Vec [] where
  before = take
  after  = drop . (+1)

inListBounds :: Pos p => p -> Bool
inListBounds p = x p >= 0 && y p >= 0

instance Matrix ListMatrix where
  type Row ListMatrix = []
  elemAt  (LM m) p | inListBounds p = Just $ m !! y p !! x p
  elemAt  _ _ = Nothing
  row     (LM m) y | y >= 0 = Just $ m !! y
  row     _ _ = Nothing
  col     (LM m) x | x >= 0 = Just $ fmap (!!x) m
  col     _ _ = Nothing
  rows    m   = m
  cols    m   = LM (catMaybes $ fmap (col m) [0..14])
  above   m p = fromMaybe [] $ before (y p) <$> col m (x p)
  below   m p = fromMaybe [] $ after  (y p) <$> col m (x p)
  leftOf  m p = fromMaybe [] $ before (x p) <$> row m (y p)
  rightOf m p = fromMaybe [] $ after  (x p) <$> row m (y p)
