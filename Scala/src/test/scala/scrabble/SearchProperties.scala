package scrabble

import org.scalacheck.Gen
import org.scalacheck.{Prop, Properties}
import org.scalacheck.Prop._
import Search._

trait UnitTest { this: Properties =>
  def unitTest(name:String)(f: => Prop) = property(name) = secure { f }
}

class ScrabbleTest(name: String) extends Properties(name) with UnitTest

object SearchProperties extends Properties("Search") {

  // strGen generates a fixed length random string
  val strGen = (n: Int) => Gen.listOfN(n, Gen.alphaChar).map(_.mkString)
  val alphaStr = Gen.listOf(Gen.alphaChar).map(_.mkString)

  val fixedLengthStr = forAll(strGen(10)){ s => s.length == 10 }

  // properties
  property("startsWith self") = forAll(alphaStr) { s => startsWith(s)(s) }

  // unit tests
  unitTest("test"){ true }

  def unitTest(name:String)(f: => Prop) = property(name) = secure { f }
}
