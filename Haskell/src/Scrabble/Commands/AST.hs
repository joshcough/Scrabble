module Scrabble.Commands.AST where

import Data.Char (toUpper)
import Scrabble.Bag
import Scrabble.Search
import Scrabble.Types
import Scrabble.Commands.SExpr
import Prelude hiding (Word)

type Regex = String

-- if you used any _ tiles, you have to specify the word.
data Placement = Placement Pattern Orientation Position (Maybe Word) deriving Show

data ScrabbleExp =
  Search SearchExp |
  Place Placement  |
  Skip             |
  Help             |
  ShowBoard Bool
  deriving (Show)

data SearchExp =
  MatchAll  [SearchExp] |
  MatchAny  [SearchExp] |
  MatchNone [SearchExp] |
  Prim PrimSearchExp
  deriving (Show)

data PrimSearchExp =
  StartsWith Pattern  |
  EndsWith   Pattern  |
  LetterAt Letter Int |
  NoneOf [Letter]     |
  AnyOf  [Letter]     |
  AllOf  [Letter]     |
  Only   [Letter]     |
  LooksLike Pattern   |
  Regex Regex
  deriving (Show)

instance FromSExpr ScrabbleExp where
  fromSExpr = f where
    f (List [AtomSym "search", s]) = Search <$> fromSExpr s
    f (List [AtomSym "place", s, o, p, AtomSym b]) =
      Place <$> (Placement <$> fromSExpr s <*> fromSExpr o <*> fromSExpr p <*> pure (Just b))
    f (List [AtomSym "place", s, o, p]) =
      Place <$> (Placement <$> fromSExpr s <*> fromSExpr o <*> fromSExpr p <*> pure Nothing)
    f (AtomSym "skip")           = return Skip
    f (AtomSym "help")           = return Help
    f (AtomSym "showBoard")      = return (ShowBoard True)
    f (AtomSym "showBoardClean") = return (ShowBoard False)
    f bad = parseError_ "bad command" bad

instance FromSExpr Position where
  fromSExpr (List [AtomNum x, AtomNum y]) =
    return $ Position (fromIntegral x) (fromIntegral y)
  fromSExpr bad = parseError_ "bad position" bad

instance FromSExpr Orientation where
  fromSExpr (AtomSym "H") = return Horizontal
  fromSExpr (AtomSym "V") = return Vertical
  fromSExpr bad = parseError_ "bad orientation" bad

instance FromSExpr SearchExp where
  fromSExpr = f where
    f (List (AtomSym "matchAll"  : rest)) = MatchAll  <$> traverse fromSExpr rest
    f (List (AtomSym "matchAny"  : rest)) = MatchAny  <$> traverse fromSExpr rest
    f (List (AtomSym "matchNone" : rest)) = MatchNone <$> traverse fromSExpr rest
    f other = Prim <$> fromSExpr other

instance FromSExpr PrimSearchExp where
  fromSExpr = f where
    f (List [AtomSym "startsWith", AtomSym p]) = StartsWith <$> fromString p
    f (List [AtomSym "endsWith",   AtomSym p]) = EndsWith   <$> fromString p
    f (List [AtomSym "letterAt",   AtomSym p, AtomNum n]) = return $ LetterAt (head p) (fromIntegral n)
    f (List [AtomSym "noneOf",     AtomSym p]) = return $ NoneOf p
    f (List [AtomSym "anyOf",      AtomSym p]) = return $ AnyOf p
    f (List [AtomSym "allOf",      AtomSym p]) = return $ AllOf p
    f (List [AtomSym "only",       AtomSym p]) = return $ Only p
    f (List [AtomSym "lookslike",  AtomSym p]) = LooksLike <$> fromString p
    f (List [AtomSym "regex",      AtomSym p]) = return $ Regex p
    f bad = parseError_ "bad search" bad

instance FromSExpr Pattern where
  fromSExpr (AtomSym p) = Pattern <$> traverse f p where
    f :: Char -> Either String (Maybe Tile)
    f '@' = return Nothing
    f  l  = maybe (Left $ "bad letter: " ++ show l) (return.return) (Tile l <$> lookup (toUpper l) points)
  fromSExpr bad = parseError_ "bad pattern" bad

toSearch1 :: SearchExp -> Either String Search1
toSearch1 (MatchAll  searches) = matchAll  <$> traverse toSearch1 searches
toSearch1 (MatchAny  searches) = matchAny  <$> traverse toSearch1 searches
toSearch1 (MatchNone searches) = matchNone <$> traverse toSearch1 searches
toSearch1 (Prim search)        = primToSearch1 search

primToSearch1 :: PrimSearchExp -> Either String Search1
primToSearch1 (StartsWith pat) = return . startsWith $ show pat
primToSearch1 (EndsWith   pat) = return . endsWith   $ show pat
primToSearch1 (LetterAt l n)   = return $ containsLetterAtPos l n
primToSearch1 (NoneOf   ls)    = return $ containsNone ls
primToSearch1 (AnyOf    ls)    = return $ containsAny  ls
primToSearch1 (AllOf    ls)    = return $ containsAll  ls
primToSearch1 (Only     ls)    = return $ containsOnly ls
primToSearch1 (LooksLike pat)  = return . looksLike $ show pat
primToSearch1 (Regex r)        = return $ regex r

