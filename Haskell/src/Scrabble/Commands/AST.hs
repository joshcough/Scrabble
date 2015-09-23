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
  ShowBoard Bool   |
  ShowScores
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
    f (List [AtomSym "place", AtomSym w, o, p, AtomSym b]) = do
      o' <- fromSExpr o
      p' <- fromSExpr p
      Place <$> makePutWord w o' p' b
    f (List [AtomSym "place", AtomSym w, o, p]) = do
      o' <- fromSExpr o
      p' <- fromSExpr p
      Place <$> makePutWord w o' p' ""
    f (AtomSym "skip")        = return Skip
    f (AtomSym "help")        = return Help
    f (AtomSym "board")       = return (ShowBoard True)
    f (AtomSym "board-clean") = return (ShowBoard False)
    f (AtomSym "scores")      = return ShowScores
    f bad                     = parseError_ "bad command" bad

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

-- TODO: check if input chars are bad
-- a lot of error handling isn't happening here
-- this code is really bad
makePutWord :: String      ->
               Orientation ->
               Position    ->
               [Char]      ->
               Either String PutWord
makePutWord w o p blanks = return $ PutWord putTils where

  coordinates :: [(Int,Int)]
  coordinates = reverse . fst $ foldl f ([],(x p,y p)) w where
    f (acc,(x,y)) c = ((x,y):acc, adder (x,y))
    adder = catOrientation (\(x,y) -> (x+1,y)) (\(x,y) -> (x,y+1)) o

  putTils :: [PutTile]
  putTils = catMaybes $ zipWith f w (zip coordinates [0..]) where
    f '@' _     = Nothing
    f '_' (p,i) = Just $ PutLetterTile (mkTile $ blanks !! i) (pos p)
    f  c  (p,i) = Just $ PutLetterTile (mkTile c) (pos p)