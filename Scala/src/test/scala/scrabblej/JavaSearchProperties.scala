package scrabblej

import collection.JavaConverters._
import org.scalacheck.Prop._
import scrabble.ScrabbleTest
import Search._

object JavaSearchProperties extends ScrabbleTest("JavaSearchProperties") {

  unitTest("hi") {
    Search.any(List(
      containsAny("xyz"),
      containsAll("hi"),
      containsOnly("hi")
    ).asJava).apply("hi")
  }

  unitTest("hi") {
    Search.all(List(
      containsAll("how"),
      containsAny(","),
      charAt(4, 'o'),
      startsWith("hello"),
      endsWith("world"),
      containsNone("xyz")
    ).asJava).apply("hello, world")
  }

  unitTest("hi") {
    Search.and(
      Search.and(containsAll("how"), containsAny(",")),
      Search.or(charAt(4, 'o'), startsWith("hello"))
    ).apply("hello, world")
  }
}
