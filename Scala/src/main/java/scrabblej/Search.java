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
}
