package scrabble

import scala.io.Source

object Search {
  type Matcher = String => Boolean
  type Word = String

  implicit class RichString(s:String) {
    def ups: String = s.up.sorted
    def up:  String = s.toUpperCase
  }

  def startsWith(s: String)(w: Word): Boolean =
    w.up.take(s.length) == s.up

  def endsWith(s: String)(w: Word): Boolean =
    startsWith(s.up.reverse)(w.up.reverse)

  def containsAny(cs: List[Char])(w: Word): Boolean = {
    val (csu, wu) = (cs.toString.ups, w.ups)
    csu.foldRight(false) { (c,acc) => acc || wu.exists(_ == c) }
  }

  def containsOnly(cs: List[Char])(w: Word): Boolean = {
    val (csu, wu) = (cs.toString.ups, w.ups)
    csu.foldRight(true) { (c,acc) => acc && wu.exists(_ == c) }
  }

  def containsNone(cs: List[Char])(w: Word): Boolean = ! containsAny(cs)(w)

  /**
   * Search for _all_ of the letters in the first string (cs).
   * If a letter appears more than once in cs, it must
   * appear more than once in w.
   * More accurately:
   *  If a char appears n times in cs,
   *  it must appear n' times in w where n' >= n
   * @param cs
   * @param w
   * @return
   */
  def containsAll(cs: List[Char])(w: Word): Boolean = {
    val (csu: String, wu: String) = (cs.mkString.ups, w.ups)
    csu.foldLeft[(Boolean, String)]((true,wu)){
      case ((b, s), c) =>
        if(s.contains(c)) (b, s.replaceFirst(c.toString, ""))
        else (false, "")
    }._1
  }

  def containsLetterAtPos(c: Char, pos: Int)(w: Word): Boolean =
    pos > 0 && pos <= w.length && w.charAt(pos).toUpper == c.toUpper

  // ------ Search Combinators ------

  def or (m1:Matcher, m2:Matcher)(w:Word): Boolean = m1(w) || m2(w)
  def and(m1:Matcher, m2:Matcher)(w:Word): Boolean = m1(w) && m2(w)
  def any(ms: List[Matcher])(w:Word): Boolean =
    ms.foldRight[Word => Boolean](Function const false)(or)(w)
  def all(ms: List[Matcher])(w:Word): Boolean =
    ms.foldRight[Word => Boolean](Function const true)(and)(w)

  // DRYing those up.
  def combine(b: Boolean,
              f: (Matcher, Matcher) => Matcher,
              ms:List[Matcher]): Matcher =
    ms.foldRight[Matcher](Function const b)(f)

  def any_dry(ms: List[Matcher]): Matcher = combine(false, or, ms)
  def all_dry(ms: List[Matcher]): Matcher = combine(true, and, ms)

  val dictionary: Set[Word] =
    Source.fromFile("../dict/en.txt").getLines.map(_.toUpperCase).toSet

  // Run a search on a whole dictionary of words
  def runMatcher(m:Matcher, in: Set[Word]): Set[Word] = in.filter(m)

  def cheat(m:Matcher): Set[Word] = runMatcher(m, dictionary)

  def timed[A](f: => A): A = {
    val now = System.currentTimeMillis
    val a = f
    println(System.currentTimeMillis - now)
    a
  }
}
