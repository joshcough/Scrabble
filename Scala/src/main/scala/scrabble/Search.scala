package scrabble

object Search {
  type Word = String

  def startsWith(s: String)(w: Word): Boolean =
    w.take(s.length) == s

  def endsWith(s: String)(w: Word): Boolean =
    startsWith(s.reverse)(w.reverse)

}
