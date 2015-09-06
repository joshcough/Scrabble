module Scrabble.Types where

type Letter = Char
type Tray   = [Letter]
type Word   = String
type Points = Int

data Orientation = Horizontal | Vertical deriving Show