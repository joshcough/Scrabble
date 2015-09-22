{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PatternSynonyms #-}
module Scrabble.Matrix
  ( Matrix(..)
  , ListMatrix
  , LM
  ) where

import Data.Functor.Compose

class Matrix r m | m -> r where
  elemAt  :: Pos p => (m a) -> p -> Maybe a
  row     :: m a -> Int -> Maybe (r a)
  col     :: m a -> Int -> Maybe (r a)
  rows    :: m a -> m a
  cols    :: m a -> m a
  above   :: Pos p => m a -> p -> r a
  below   :: Pos p => m a -> p -> r a
  leftOf  :: Pos p => m a -> p -> r a
  rightOf :: Pos p => m a -> p -> r a

-- | The composition of two lists forms a "matrix".
type ListMatrix = Compose [] []

-- | A shorthand for getting the list-of-list out of a 'ListMatrix',
-- and putting one back in.
pattern LM a = Compose a

instance Matrix [] ListMatrix where
  elemAt  (LM m) p | x >= 0 && y >= 0 = Just (m !! y !! x) where (x,y) = coors p
  elemAt  _ _ = Nothing
  row     (LM m) y | y >= 0 = Just $ m !! y
  row     _ _ = Nothing
  col     (LM m) x | x >= 0 = Just $ fmap (!!x) m
  col     _ _ = Nothing
  rows    m   = m
  cols    m   = Maybe.catMaybes $ fmap (col m) [0..14]
  above   m p = Maybe.fromMaybe [] $ before (y p) <$> col m (x p)
  below   m p = Maybe.fromMaybe [] $ after  (y p) <$> col m (x p)
  leftOf  m p = Maybe.fromMaybe [] $ before (x p) <$> row m (y p)
  rightOf m p = Maybe.fromMaybe [] $ after  (x p) <$> row m (y p)

