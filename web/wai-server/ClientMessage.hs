{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module ClientMessage where

import Data.Aeson
import Scrabble
import GHC.Generics


data MessageType = ValidityCheck | ActualMove
    deriving (Show, Eq, Generic, ToJSON, FromJSON)

data ClientMessage = Message MessageType Game WordPut
    deriving (Eq, Generic, ToJSON, FromJSON)
