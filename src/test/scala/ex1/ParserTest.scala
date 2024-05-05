package ex1

import ex1.Parsers.charParser
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ParserTest extends AnyFunSuite with Matchers:
  def parser    = new BasicParser(Set('a', 'b', 'c'))
  def parserNE  = new NonEmptyParser(Set('0', '1'))
  def parserNTC = new NotTwoConsecutiveParser(Set('X', 'Y', 'Z'))
  def parserNTCNE = new BasicParser(Set('X', 'Y', 'Z'))
    with NotTwoConsecutive[Char]
    with NonEmpty[Char]
  def sparser: Parser[Char] = "abc".charParser
  def parserSTN             = new BasicParser(Set('X', 'Y', 'Z')) with ShorterThanN[Char](n = 5)

  test("test BasicParser"):
    parser.parseAll("aabc".toList) should be(true)
    parser.parseAll("aabcdc".toList) should be(false)
    parser.parseAll("".toList) should be(true)

  test("test NotEmpty parser"):
    parserNE.parseAll("0101".toList) should be(true)
    parserNE.parseAll("0123".toList) should be(false)
    parserNE.parseAll(List()) should be(false)

  test("test NotTwoConsecutive parser"):
    parserNTC.parseAll("XYZ".toList) should be(true)
    parserNTC.parseAll("XYYZ".toList) should be(false)
    parserNTC.parseAll("".toList) should be(true)

  test("test NotEmpty and NotTwoConsecutive parser"):
    parserNTCNE.parseAll("XYZ".toList) should be(true)
    parserNTCNE.parseAll("XYYZ".toList) should be(false)
    parserNTCNE.parseAll("".toList) should be(false)

  test("test string parser"):
    sparser.parseAll("aabc".toList) should be(true)
    sparser.parseAll("aabcdc".toList) should be(false)
    sparser.parseAll("".toList) should be(true)

  test("test ShorterThenN parser"):
    parserSTN.parseAll("XYZXY".toList) should be(true)
    parserSTN.parseAll("XYZXYZ".toList) should be(false)
    parserSTN.parseAll("".toList) should be(true)
