module Scrabble.Commands.AST where

import Data.Char (toUpper)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Debug.Trace
import Scrabble.Bag
import Scrabble.Search
import Scrabble.Types
import Scrabble.Commands.SExpr
import Prelude hiding (Word)

type Regex = String

data ScrabbleExp =
  Search  SearchExp |
  Place   PutWord   |
  Skip              |
  ShowExp ShowExp -- TODO rename to ShowBoard or something
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

data ShowExp = ShowBoard Bool | ShowScores | ShowHelp
  deriving Show

instance FromSExpr ScrabbleExp where
  fromSExpr exp@(List l) = f l where
    f [AtomSym "search", s]    = Search <$> fromSExpr s
    f (AtomSym "place" : info) = parsePlace info
    f [AtomSym "skip"]         = return Skip
    f (AtomSym "show"  : exps) = parseShowExps (showSExpr_ <$> exps)
    f bad                      = parseError_ "bad command" exp

parseShowExps :: [String] -> Either String ScrabbleExp
parseShowExps = p where
  p ["board"]          = rs $ ShowBoard True
  p ["board", "clean"] = rs $ ShowBoard False
  p ["help"]           = rs $ ShowHelp
  p ["scores"]         = rs $ ShowScores
  p bad                = parseError "bad show command" (show bad)
  rs = return . ShowExp

parsePlace :: [SExpr] -> Either String ScrabbleExp
parsePlace l = parsePlace' l where
  parsePlace' [AtomSym w, o, p, AtomSym b] = f w o p b
  parsePlace' [AtomSym w, o, p]            = f w o p ""
  parsePlace  bad = parseError_ "bad placement" bad
  f w o p b = do
    o' <- fromSExpr o
    p' <- fromSExpr p
    Place <$> makePutWord w o' p' b

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
    f (List (AtomSym "matchAll"  : rest)) = MatchAll  <$> t rest
    f (List (AtomSym "matchAny"  : rest)) = MatchAny  <$> t rest
    f (List (AtomSym "matchNone" : rest)) = MatchNone <$> t rest
    f other = Prim <$> fromSExpr other
    t = traverse fromSExpr

searchKeyWords :: Map String (String -> PrimSearchExp)
searchKeyWords = Map.fromList [
  ("startsWith",StartsWith)
 ,("endsWith"  ,EndsWith)
 ,("noneOf"    ,NoneOf)
 ,("anyOf"     ,AnyOf)
 ,("allOf"     ,AllOf)
 ,("only"      ,Only)
 ,("regex"     ,Regex)
 ,("looksLike" ,LooksLike) ]

instance FromSExpr PrimSearchExp where
  fromSExpr exp@(List l) = f l where
    f [AtomSym "letterAt", AtomSym p, AtomNum n] =
      return $ LetterAt (head p) (fromIntegral n)
    f [AtomSym kw, AtomSym s]                    =
      maybe err (\g -> return $ g s) (Map.lookup kw searchKeyWords)
    f   _ = err
    err = parseError_ "bad search" exp

toSearch1 :: SearchExp -> Either String Search1
toSearch1 = f where
  f (MatchAll  searches) = matchAll  <$> t searches
  f (MatchAny  searches) = matchAny  <$> t searches
  f (MatchNone searches) = matchNone <$> t searches
  f (Prim search)        = return $ primToSearch1 search
  t = traverse toSearch1

primToSearch1 :: PrimSearchExp -> Search1
primToSearch1 (StartsWith pat) = startsWith $ show pat
primToSearch1 (EndsWith   pat) = endsWith   $ show pat
primToSearch1 (LetterAt   l n) = containsLetterAtPos l n
primToSearch1 (NoneOf     ls)  = containsNone ls
primToSearch1 (AnyOf      ls)  = containsAny  ls
primToSearch1 (AllOf      ls)  = containsAll  ls
primToSearch1 (Only       ls)  = containsOnly ls
primToSearch1 (LooksLike  pat) = looksLike $ show pat
primToSearch1 (Regex r)        = regex r

-- TODO: check if input chars are bad
-- a lot of error handling isn't happening here
-- this code is really bad
makePutWord :: String      ->
               Orientation ->
               Position    ->
               [Char]      ->
               Either String PutWord
makePutWord w o p blanks = return $ PutWord putTils where

  coords :: [(Int,Int)]
  coords = reverse . fst $ foldl f ([],coors p) w where
    f (acc,p) c = (p:acc, catOrientation rightOfP belowP o p)

  putTils :: [PutTile]
  putTils = catMaybes $ zipWith f w (zip coords [0..]) where
    f '@' _     = Nothing
    f '_' (p,i) = plt (mkTile $ blanks !! i) (pos p)
    f  c  (p,i) = plt (mkTile c) (pos p)
    plt t p     = Just $ PutLetterTile t p