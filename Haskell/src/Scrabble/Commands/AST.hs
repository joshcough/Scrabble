module Scrabble.Commands.AST where

import Data.Char (toUpper)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Scrabble.Bag
import Scrabble.Search
import Scrabble.Types
import Scrabble.Commands.SExpr
import Prelude hiding (Word)

type Regex = String

data ScrabbleExp =
  Search SearchExp |
  Place  PutWord   |
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
  StartsWith String   |
  EndsWith   String   |
  LetterAt Letter Int |
  NoneOf [Letter]     |
  AnyOf  [Letter]     |
  AllOf  [Letter]     |
  Only   [Letter]     |
  LooksLike String    |
  Regex Regex
  deriving (Show)

instance FromSExpr ScrabbleExp where
  fromSExpr = f where
    f (List [AtomSym "search", s]) = Search <$> fromSExpr s
    f (List [AtomSym "place", AtomSym w, o, p, AtomSym b]) =
      Place <$> (PutWord <$> g w b   <*> fromSExpr o <*> fromSExpr p)
    f (List [AtomSym "place", AtomSym w, o, p]) =
      Place <$> (PutWord <$> g w ""  <*> fromSExpr o <*> fromSExpr p)
    f (AtomSym "skip")           = return Skip
    f (AtomSym "help")           = return Help
    f (AtomSym "showBoard")      = return (ShowBoard True)
    f (AtomSym "showBoardClean") = return (ShowBoard False)
    f bad = parseError_ "bad command" bad
    -- TODO: check if input chars are bad
    -- a lot of error handling isn't happening here
    -- this code is really bad
    g :: String -> String -> Either String PutTiles
    g w b = return . PutTiles . reverse . fst $ foldl h ([],b) w where
      h :: ([Maybe PutTile], [Letter]) -> Letter -> ([Maybe PutTile], [Letter])
      h (acc,r:rs) c | c == '_' || c == ' ' = (Just (PutBlankTile r) : acc, rs)
      h (acc,rs)   c | c == '@' = (Nothing : acc, rs)
      h (acc,rs)   c = (Just (PutLetterTile $ mkTile c) : acc, rs)

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

searchKeyWords :: Map String (String -> PrimSearchExp)
searchKeyWords = Map.fromList [
  ("startsWith",StartsWith), ("endsWith",EndsWith), ("noneOf",NoneOf), ("anyOf",AnyOf)
 ,("allOf",AllOf), ("only",Only), ("regex",Regex), ("looksLike",LooksLike) ]

instance FromSExpr PrimSearchExp where
  fromSExpr = f where
    f   (List [AtomSym "letterAt", AtomSym p, AtomNum n]) = return $ LetterAt (head p) (fromIntegral n)
    f l@(List [AtomSym kw, AtomSym s]) = maybe (bad l) (\g -> return $ g s) (Map.lookup kw searchKeyWords)
    f x = bad x
    bad x = parseError_ "bad search" x

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

