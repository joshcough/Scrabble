{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module ClientMessage where

import Data.Aeson
import Scrabble
import GHC.Generics


data MessageType = ValidityCheck | ActualMove
    deriving (Eq, Generic, FromJSON)

data ClientMessage = Message MessageType Game WordPut
    deriving (Eq, Generic, FromJSON)
