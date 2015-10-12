package scrabblej;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

public class Search {

  interface Matcher {
    boolean apply(String s);
  }
  
  /**
   * Search for _all_ of the letters in the first string (s1).
   * If a letter appears more than once in s1, it must
   * appear more than once in s2.
   * More accurately:
   * If a letter appears n times in s1,
   * it must appear n' times in s2 where n' >= n
   * @param s1
   * @return
   */
  public static Matcher containsAll(final String s1) {
    return new Matcher() {
      private boolean apply(String a, String b) {
        if     (a.isEmpty()) return true;
        else if(b.isEmpty()) return false;
        else if(a.charAt(0) == b.charAt(0))
          return apply(a.substring(1), b.substring(1));
        else
          return apply(a, b.substring(1));
      }
      public boolean apply(String s2) {
        return apply(upSort(s1), upSort(s2));
      }
    };
  }

  public static Matcher containsAny(final String s1) {
    return new Matcher() {
      private boolean apply(String a, String b) {
        if     (a.isEmpty()) return true;
        else if(b.isEmpty()) return false;
        else if(a.charAt(0) == b.charAt(0)) return true;
        else return apply(a, b.substring(1));
      }
      public boolean apply(String s2) {
        return apply(upSort(s1), upSort(s2));
      }
    };
  }

  public static Matcher containsOnly(final String s1) {
    return new Matcher() {
      public boolean apply(String s2) {
        return upSort(s1).equals(upSort(s2));
      }
    };
  }

  public static Matcher containsNone(final String s1) {
    return new Matcher() {
      public boolean apply(String s2) {
        return ! containsNone(s1).apply(s2);
      }
    };
  }

  public static Matcher charAt(final char c, final int i) {
    return new Matcher() {
      public boolean apply(String s) {
        return upSort(s).charAt(i) == Character.toUpperCase(c);
      }
    };
  }

  public static Matcher endsWith(final String s1) {
    return new Matcher() {
      public boolean apply(String s2) {
        return upSort(s2).endsWith(upSort(s1));
      }
    };
  }

  public static Matcher startsWith(final String s1) {
    return new Matcher() {
      public boolean apply(String s2) {
        return upSort(s2).endsWith(upSort(s1));
      }
    };
  }

  public static Matcher or(final Matcher m1, final Matcher m2) {
    return new Matcher() {
      public boolean apply(String s) {
        return m1.apply(s) || m2.apply(s);
      }
    };
  }

  public static Matcher and(final Matcher m1, final Matcher m2) {
    return new Matcher() {
      public boolean apply(String s) {
        return m1.apply(s) && m2.apply(s);
      }
    };
  }

  public static Matcher any(final List<Matcher>  ms) {
    return new Matcher() {
      public boolean apply(String s) {
        if(ms.size()==0) return false;
        else {
          List<Matcher> copy = ms.subList(1);
          return ms.get(0) || any(lms.remove(0)).apply(lms.toArray());
        }
      }
    };
  }

  public static String upSort(String s) {
    char[] chars = s.toUpperCase().toCharArray();
    Arrays.sort(chars);
    return new String(chars);
  }
}

/**
 ------ Search combinators ------

 any' :: [Matcher] -> Matcher
 any' = foldr or (const False)

 all' :: [Matcher] -> Matcher
 all' = foldr and (const True)

 combine :: Bool                ->
 (Matcher            ->
 Matcher -> Matcher) ->
 [Matcher]           ->
 Matcher
 combine b f = foldr f (const b)

 any :: [Matcher] -> Matcher
 any = combine False or

 all :: [Matcher] -> Matcher
 all = combine True and

 none :: [Matcher] -> Matcher
 none ss w = not (all ss w)

 matchAll  = Scrabble.Search.all
 matchAny  = Scrabble.Search.any
 matchNone = Scrabble.Search.none

 ------ Dictionary Searching ---------

 dictionary :: IO Dict
 dictionary = do
 d <- readFile "../dict/en.txt"
 return $ Set.fromList (fmap toUpper <$> lines d)

 dictionaryUnsafe = unsafePerformIO dictionary

 -- Run a search on a whole dictionary of words
 runMatcher :: Matcher -> Set Word -> Set Word
 runMatcher s = Set.filter s

 cheat :: Matcher -> IO (Set Word)
 cheat search = runMatcher search <$> dictionary

 dictContainsWord :: Dict -> Word -> Bool
 dictContainsWord d = flip Set.member d . ups

 powerset :: [a] -> [[a]]
 powerset = filterM (const [True, False])

 searchDictForAllWords :: Dict -> Set String -> Set String
 searchDictForAllWords d s = Set.intersection d s

 searchDictForPowerset :: Dict -> [Char] -> Set String
 searchDictForPowerset d = searchDictForAllWords d . permset

 searchDictForPowersetSorted :: Dict -> [Char] -> [String]
 searchDictForPowersetSorted d s =
 sort . Set.toList $ searchDictForPowerset d s

 permset :: [Char] -> Set String
 permset s = Set.fromList $ concat (permutations <$> powerset s)

 {- A quick utility to search the dictionary
 for all possible words make with the given rack. -}
 testSearch :: [Char] -> IO [Word]
 testSearch rack = do
 d <- dictionary
 return $ searchDictForPowersetSorted d rack

 shuffle xs [] = [xs]
 shuffle [] ys = [ys]
 shuffle (x:xs) (y:ys) =
 map (x:) (shuffle xs (y:ys))
 ++ map (y:) (shuffle (x:xs) ys)

 anagrams = foldrM shuffle "" . group . sort

 subanagrams = foldrM f "" . map tails . group . sort where
 f is j = is >>= flip shuffle j
**/