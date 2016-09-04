package fastparse

import fastparse.Utils.IsReachable
import fastparse.core.ParseCtx

object GenericIntegerParser{
  type Parser[+T] = fastparse.core.Parser[T, Byte, Array[Byte]]
}
import GenericIntegerParser._

class GenericIntegerParser[T](n: Int, creator: (IsReachable[Byte], Int) => T)
                             (implicit name: sourcecode.Name) extends Parser[T]{

  override def toString = name.value

  def parseRec(cfg: ParseCtx[Byte, Array[Byte]], index: Int) = {
    if (!cfg.input.isReachable(n + index)) fail(cfg.failure, index)
    else success(cfg.success, creator(cfg.input, index), index + n, Set.empty, false)

  }
}
trait EndianByteParsers {
  /**
    * Parses an 8-bit signed Byte
    */
  val Int8: Parser[Byte] = new GenericIntegerParser(1, (input, n) => input(n))
  /**
    * Parses an 16-bit signed Short
    */
  val Int16: Parser[Short]
  /**
    * Parses an 32-bit signed Int
    */
  val Int32: Parser[Int]
  /**
    * Parses an 64-bit signed Long
    */
  val Int64: Parser[Long]

  /**
    * Parses an 8-bit un-signed Byte, stuffed into a Short
    */
  val UInt8: Parser[Short] = new GenericIntegerParser(1, (input, n) => (input(n) & 0xff).toShort)
  /**
    * Parses an 16-bit signed Short, stuffed into an Int
    */
  val UInt16: Parser[Int]
  /**
    * Parses an 32-bit signed Int, stuffed into a Long
    */
  val UInt32: Parser[Long]

  /**
    * Parses an 32-bit signed Float
    */
  val Float32: Parser[Float]
  /**
    * Parses an 32-bit signed Double
    */
  val Float64: Parser[Double]
}
object EndianByteParsers{
  /**
    * Parsers for parsing 16, 32 and 64 bit integers in little-endian format
    */
  object LE extends EndianByteParsers {

    val Int16: Parser[Short] = new GenericIntegerParser(2, (input, n) =>
      (((input(n+1) & 0xff) << 8) | (input(n) & 0xff)).toShort
    )

    val Int32: Parser[Int] = new GenericIntegerParser(4, (input, n) =>
      ((input(n+3) & 0xff) << 24) | ((input(n+2) & 0xff) << 16) |
      ((input(n+1) & 0xff) << 8) | (input(n) & 0xff)
    )

    val Int64: Parser[Long] = new GenericIntegerParser(8, (input, n) =>
      ((input(n+7) & 0xffL) << 54) | ((input(n+6) & 0xffL) << 48) |
      ((input(n+5) & 0xffL) << 40) | ((input(n+4) & 0xffL) << 32 ) |
      ((input(n+3) & 0xffL) << 24) | ((input(n+2) & 0xffL) << 16) |
      ((input(n+1) & 0xffL) << 8) | (input(n) & 0xffL)
    )

    val UInt16: Parser[Int] = new GenericIntegerParser(2, (input, n) =>
      ((input(n+1) & 0xff) << 8) | (input(n) & 0xff)
    )
    val UInt32: Parser[Long] = new GenericIntegerParser(4, (input, n) =>
      ((input(n+3) & 0xffL) << 24) | ((input(n+2) & 0xffL) << 16) |
        ((input(n+1) & 0xffL) << 8) | (input(n) & 0xffL)
    )

    val Float32: Parser[Float] = new GenericIntegerParser(4, (input, n) =>
      java.lang.Float.intBitsToFloat(
        ((input(n+3) & 0xff) << 24) | ((input(n+2) & 0xff) << 16) |
          ((input(n+1) & 0xff) << 8) | (input(n) & 0xff)
      )
    )

    val Float64: Parser[Double] = new GenericIntegerParser(8, (input, n) =>
      java.lang.Double.longBitsToDouble(
        ((input(n+7) & 0xffL) << 54) | ((input(n+6) & 0xffL) << 48) |
        ((input(n+5) & 0xffL) << 40) | ((input(n+4) & 0xffL) << 32 ) |
        ((input(n+3) & 0xffL) << 24) | ((input(n+2) & 0xffL) << 16) |
        ((input(n+1) & 0xffL) << 8) | (input(n) & 0xffL)
      )
    )
  }
  /**
    * Parsers for parsing 16, 32 and 64 bit integers in big-endian format
    */
  object BE extends EndianByteParsers {
    val Int16: Parser[Short] = new GenericIntegerParser(2, (input, n) =>
      (((input(n) & 0xff) << 8) | (input(n+1) & 0xff)).toShort
    )
    val Int32: Parser[Int] = new GenericIntegerParser(4, (input, n) =>
      ((input(n) & 0xff) << 24) | ((input(n+1) & 0xff) << 16) |
      ((input(n+2) & 0xff) << 8) | (input(n+3) & 0xff)
    )
    val Int64: Parser[Long] = new GenericIntegerParser(8, (input, n) =>
      ((input(n) & 0xffL) << 54) | ((input(n+1) & 0xffL) << 48) |
      ((input(n+2) & 0xffL) << 40) | ((input(n+3) & 0xffL) << 32 ) |
      ((input(n+4) & 0xffL) << 24) | ((input(n+5) & 0xffL) << 16) |
      ((input(n+6) & 0xffL) << 8) | (input(n+7) & 0xffL)
    )
    val UInt16: Parser[Int] = new GenericIntegerParser(2, (input, n) =>
      ((input(n) & 0xff) << 8) | (input(n+1) & 0xff)
    )
    val UInt32: Parser[Long] = new GenericIntegerParser(4, (input, n) =>
      ((input(n) & 0xffL) << 24) | ((input(n+1) & 0xffL) << 16) |
      ((input(n+2) & 0xffL) << 8) | (input(n+3) & 0xffL)
    )
    val Float32: Parser[Float] = new GenericIntegerParser(4, (input, n) =>
      java.lang.Float.intBitsToFloat(
        ((input(n) & 0xff) << 24) | ((input(n+1) & 0xff) << 16) |
        ((input(n+2) & 0xff) << 8) | (input(n+3) & 0xff)
      )
    )
    val Float64: Parser[Double] = new GenericIntegerParser(8, (input, n) =>
      java.lang.Double.longBitsToDouble(
        ((input(n) & 0xffL) << 54) | ((input(n+1) & 0xffL) << 48) |
        ((input(n+2) & 0xffL) << 40) | ((input(n+3) & 0xffL) << 32 ) |
        ((input(n+4) & 0xffL) << 24) | ((input(n+5) & 0xffL) << 16) |
        ((input(n+6) & 0xffL) << 8) | (input(n+7) & 0xffL)
      )
    )
  }
}
