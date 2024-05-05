package ex1

/** Consider the Parser example shown in previous lesson. Analogously to NonEmpty, create a mixin
  * NotTwoConsecutive, which adds the idea that one cannot parse two consecutive elements which are
  * equal. Use it (as a mixin) to build class NotTwoConsecutiveParser, used in the testing code at
  * the end. Note we also test that the two mixins can work together!!
  */

abstract class Parser[T]:
  def parse(t: T): Boolean // is the token accepted?
  def end: Boolean         // is it ok to end here
  def parseAll(seq: Seq[T]): Boolean = (seq forall parse) & end // note &, not &&

object Parsers:
  extension (s: String) def charParser: Parser[Char] = new BasicParser(s.toSet)

class BasicParser(chars: Set[Char]) extends Parser[Char]:
  override def parse(t: Char): Boolean = chars.contains(t)
  override def end: Boolean            = true

trait NonEmpty[T] extends Parser[T]:
  private var empty = true
  abstract override def parse(t: T): Boolean =
    empty = false
    super.parse(t) // who is super??
  abstract override def end: Boolean = !empty && super.end

class NonEmptyParser(chars: Set[Char]) extends BasicParser(chars) with NonEmpty[Char]

trait NotTwoConsecutive[T] extends Parser[T]:
  private var lastChar: Option[T] = Option.empty
  abstract override def parse(t: T): Boolean = lastChar match
    case Some(c) if c == t => false
    case _                 => lastChar = Some(t); super.parse(t)

class NotTwoConsecutiveParser(chars: Set[Char])
    extends BasicParser(chars)
    with NotTwoConsecutive[Char]

trait ShorterThanN[T](n: Int) extends Parser[T]:
  private var parsedChars = 0
  abstract override def parse(t: T): Boolean =
    parsedChars = parsedChars + 1
    super.parse(t)
  abstract override def end: Boolean = parsedChars <= n

@main def checkParsers(): Unit =
  import Parsers.charParser

  def parser = BasicParser(Set('a', 'b', 'c'))
  println(parser.parseAll("aabc".toList))   // true
  println(parser.parseAll("aabcdc".toList)) // false
  println(parser.parseAll("".toList))       // true

  // Note NonEmpty being "stacked" on to a concrete class
  // Bottom-up decorations: NonEmptyParser -> NonEmpty -> BasicParser -> Parser
  def parserNE = NonEmptyParser(Set('0', '1'))
  println(parserNE.parseAll("0101".toList)) // true
  println(parserNE.parseAll("0123".toList)) // false
  println(parserNE.parseAll("".toList))     // false

  def parserNTC = NotTwoConsecutiveParser(Set('X', 'Y', 'Z'))
  println(parserNTC.parseAll("XYZ".toList))  // true
  println(parserNTC.parseAll("XYYZ".toList)) // false
  println(parserNTC.parseAll("".toList))     // true

  // note we do not need a class name here, we use the structural type
  // parserNTCNE linearization:
  //    parserNTCNE -> NonEmpty -> NotTwoConsecutive -> BasicParser -> Parser
  def parserNTCNE = new BasicParser(Set('X', 'Y', 'Z'))
    with NotTwoConsecutive[Char]
    with NonEmpty[Char]
  println(parserNTCNE.parseAll("XYZ".toList))  // true
  println(parserNTCNE.parseAll("XYYZ".toList)) // false
  println(parserNTCNE.parseAll("".toList))     // false

  def sparser: Parser[Char] = "abc".charParser
  println(sparser.parseAll("aabc".toList))   // true
  println(sparser.parseAll("aabcdc".toList)) // false
  println(sparser.parseAll("".toList))       // true

  val maxParsedChars = 5
  def parserSTN      = new BasicParser(Set('X', 'Y', 'Z')) with ShorterThanN[Char](maxParsedChars)
  println(parserSTN.parseAll("XYZXY".toList))  // true
  println(parserSTN.parseAll("XYZXYZ".toList)) // false
  println(parserSTN.parseAll("".toList))       // true
