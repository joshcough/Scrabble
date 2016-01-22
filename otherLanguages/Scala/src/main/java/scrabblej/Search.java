package scrabblej;

import java.util.*;

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
        return ! containsAny(s1).apply(s2);
      }
    };
  }

  public static Matcher charAt(final int i, final char c) {
    return new Matcher() {
      public boolean apply(String s) {
        return up(s).charAt(i) == Character.toUpperCase(c);
      }
    };
  }

  public static Matcher endsWith(final String s1) {
    return new Matcher() {
      public boolean apply(String s2) {
        return up(s2).endsWith(up(s1));
      }
    };
  }

  public static Matcher startsWith(final String s1) {
    return new Matcher() {
      public boolean apply(String s2) {
        return up(s2).endsWith(up(s1));
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
        else return
          ms.get(0).apply(s) || any(ms.subList(1,ms.size())).apply(s);
      }
    };
  }

  public static Matcher all(final List<Matcher>  ms) {
    return new Matcher() {
      public boolean apply(String s) {
        if(ms.size()==0) return true;
        else return
          ms.get(0).apply(s) && any(ms.subList(1,ms.size())).apply(s);
      }
    };
  }

  public static String upSort(String s) {
    char[] chars = s.toUpperCase().toCharArray();
    Arrays.sort(chars);
    return new String(chars);
  }

  public static String up(String s) {
    return s.toUpperCase();
  }



  ///////////////////////



  // 1. find me all the words that contain a
  public static List<String> containsA(List<String> dictionary) {
    List<String> matches = new ArrayList<>();
    for(String s: dictionary){
      if(s.contains("a")) matches.add(s);
    }
    return matches;
  }

  // 1. find me all the words that contain a
  // 2. and find all the words that end with s.
  public static List<String> containA_and_endsWithS(List<String> dictionary) {
    List<String> matches = containsA(dictionary);
    for(String s: matches){
      if(s.endsWith("s")) matches.add(s);
    }
    return matches;
  }

  // a slightly better way to do this might be...
  // to define this...
  public static List<String> endsWithS(List<String> dictionary) {
    List<String> matches = new ArrayList<>();
    for(String s: dictionary){
      if(s.endsWith("s")) matches.add(s);
    }
    return matches;
  }

  // and then use function composition
  public static List<String> containA_and_endsWithS_2(List<String> dictionary) {
    return endsWithS(containsA(dictionary));
  }

  // but now, what if we want or?
  public static List<String> containA_or_endsWithS(List<String> dictionary) {
    List<String> a = containsA(dictionary);
    a.addAll(endsWithS(dictionary));
    return new ArrayList<>(new HashSet<>(a));
  }

  /*
    this is getting ugly...
    lets ask questions...
      what if you wanted a different character than a?
      what if you wanted a different character than s?
      what if you wanted yet another filter,
        and sometimes wanted it, but sometimes not.

        for example sometimes you want to find words that start with 'c',
        but, sometimes you dont.

     the goal is to get to a system that allows you to pick and choose
     from any of these things, and combine them with and, or, all, none, etc.
   */

  // well now what if we want some letter other than 'a'?
  public static List<String> containsChar(List<String> dictionary, char c) {
    List<String> matches = new ArrayList<>();
    for(String s: dictionary){
      if(s.contains("" + c)) matches.add(s);
    }
    return matches;
  }

  // ok, but... our containsA was trapped inside containA_and_endsWithS
  // how would we use containsChar instead?
  public static List<String> containChar_and_endsWithS(
    List<String> dictionary, char c) {
    List<String> matches = containsChar(dictionary, c);
    for(String s: matches){
      if(s.endsWith("s")) matches.add(s);
    }
    return matches;
  }

  // but we still have the "s" hardcoded. can we get rid of that?
  // ok, but... our containsA was trapped inside containA_and_endsWithS
  // how would we use containsChar instead?
  public static List<String> containChar_and_endsWithChar(
    List<String> dictionary, char c1, char c2) {
    List<String> matches = containsChar(dictionary, c1);
    for(String s: matches){
      if(s.endsWith("" + c2)) matches.add(s);
    }
    return matches;
  }

  public static List<String> endsWithChar(List<String> dictionary, char c) {
    List<String> matches = new ArrayList<>();
    for(String s: dictionary){
      if(s.endsWith("" + c)) matches.add(s);
    }
    return matches;
  }

  public static List<String> containChar_and_endsWithChar_2(
    List<String> dictionary, char c1, char c2) {
    List<String> matches = containsChar(dictionary, c1);
    // TODO: what? it's not even worth writing.
    return matches;
  }

  // but, what if we want to do "or"?
  public static List<String> containChar_or_endsWithChar(
    List<String> dictionary, char c1, char c2) {
    List<String> matches = containsChar(dictionary, c1);
    matches.addAll(endsWithChar(dictionary, c2));
    return new ArrayList<>(new HashSet<>(matches));
  }

  // what if we add a third thing?
}
