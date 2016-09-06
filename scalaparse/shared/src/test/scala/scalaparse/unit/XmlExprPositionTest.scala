package scalaparse.unit

import scalaparse.PositionRange
import scalaparse.Scala

import utest.TestSuite
import utest._

object XmlExprPositionTest extends TestSuite {

  def check(input: String, exprs: String*): Unit = {
    val s = new Scala
    val expected = exprs.toSet
    s.XmlExpr.parse(input).get
    s.getScalaExprPositions.foreach {
      case PositionRange(from, to) =>
        val capturedSubstring = input.substring(from, to)
        assert(expected.contains(capturedSubstring))
    }
  }

  println("running")
  def tests = TestSuite {
    * - {
      check(
        """<div href={url}> Hello {name + "idiot"}</div>
           """,
        "{url}",
        """{name + "idiot"}"""
      )
    }

    * - {
      check(
        """<div
              href={
              url

              }> Hello {name match {
                case Some(x) =>
                  case class Err(msg: String)
                 Err("msg")
                case _ => "banana"
              }}</div>
          """,
        """{
              url

              }""",
        """{name match {
                case Some(x) =>
                  case class Err(msg: String)
                 Err("msg")
                case _ => "banana"
              }}"""
      )
    }
  }
}
