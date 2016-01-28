{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Scrabble.SearchTests (tests) where

import Data.Monoid (mempty)
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import Scrabble.Search
import System.IO.Unsafe
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.TH
import Test.QuickCheck
import Test.QuickCheck.Instances.Char
import Test.HUnit

dict = unsafePerformIO dictionary

string = listOf lowerAlpha
search = searchWordBagForPowersetSorted

prop_startsWith_self     = forAll string $ \s -> startsWith  s s
prop_containsAll_self    = forAll string $ \s -> containsAll s s
prop_containsAll_reverse = forAll string $ \s -> containsAll s (reverse s)
prop_containsAll_implies_containsAny =
  forAll string $ \s -> containsAll  "ab" s ==> containsAny "ab" s
prop_containsNone_implies_not_containsAny =
  forAll string $ \s -> containsNone "ab" s ==> not $ containsAny "ab" s

case_startsWith     = startsWith   "hello" "hello, world" @?= True
case_endsWith       = endsWith     "world" "hello, world" @?= True
case_containsOnly   = containsOnly "abc"   "abc"          @?= True
case_search_ANIMALS = search dict "ANIMALS" @?= searchANIMALS
case_search_ANSXYZQ = search dict "ANSXYZQ" @?= searchANSXYZQ
case_search_SMITH   = search dict "SMITH"   @?= searchSMITH
case_z_and_w_at_4   = runSearch
  (matchAll [containsAny "z", containsLetterAtPos 'w' 4]) dict @?= searchZandW4

searchZandW4 = Set.fromList [
   "BUZZWIG","BUZZWIGS","BUZZWORD","BUZZWORDS","UNBOWDLERIZED",
   "ZUGZWANG","ZUGZWANGED","ZUGZWANGING","ZUGZWANGS"]

searchANIMALS =
  ["AA","AAL","AALS","AAS","AI","AIA","AIAS","AIL","AILS","AIM","AIMS",
   "AIN","AINS","AIS","AL","ALA","ALAN","ALANS","ALAS","ALIAS","ALISMA",
   "ALMA","ALMAIN","ALMAINS","ALMAS","ALMS","ALS","AM","AMA","AMAIN",
   "AMAS","AMI","AMIA","AMIAS","AMIN","AMINS","AMIS","AMLA","AMLAS",
   "AMNIA","AN","ANA","ANAL","ANAS","ANI","ANIL","ANILS","ANIMA",
   "ANIMAL","ANIMALS","ANIMAS","ANIS","ANLAS","ANSA","AS","IN","INS",
   "IS","ISM","ISNA","LA","LAIN","LAM","LAMA","LAMAS","LAMIA","LAMIAS",
   "LAMINA","LAMINAS","LAMS","LANA","LANAI","LANAIS","LANAS","LAS","LI",
   "LIANA","LIANAS","LIAS","LIMA","LIMAN","LIMANS","LIMAS","LIMN","LIMNS",
   "LIN","LINS","LIS","MA","MAA","MAAS","MAIL","MAILS","MAIN","MAINS","MAL",
   "MALA","MALAS","MALI","MALIS","MALS","MAN","MANA","MANAS","MANI","MANIA",
   "MANIAS","MANILA","MANILAS","MANIS","MANS","MAS","MASA","MASLIN","MI",
   "MIL","MILS","MINA","MINAS","MIS","MNA","MNAS","NA","NAAM","NAAMS",
   "NAIL","NAILS","NALA","NALAS","NAM","NAMS","NAS","NASAL","NASIAL",
   "NIL","NILS","NIM","NIMS","NIS","SAI","SAIL","SAIM","SAIN","SAL",
   "SALAMI","SALINA","SALMI","SAM","SAMA","SAMAN","SAN","SI","SIAL",
   "SIM","SIMA","SIN","SLAIN","SLAM","SLIM","SMA","SNAIL"]

searchANSXYZQ =
  ["AN","ANY","AS","AX","AY","AYS","NA","NAS","NAY","NAYS","NY","NYAS",
   "NYS","SAN","SAX","SAY","SAZ","SNY","SYN","YA","ZA","ZANY","ZAS","ZAX"]

searchSMITH =
  ["HI","HIM","HIMS","HIS","HIST","HIT","HITS","HM","IS","ISH","ISM",
   "IT","ITS","MI","MIS","MIST","SH","SHIM","SHIT","SI","SIM","SIT",
   "SITH","SMIT","SMITH","ST","STIM","THIS","TI","TIS"]

tests = $testGroupGenerator