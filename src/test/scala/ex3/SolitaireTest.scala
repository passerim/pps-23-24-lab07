package ex3

import ex3.Solitaire.{*, given}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SolitaireTest extends AnyFlatSpec with Matchers:
  "A 5x5 solitaire game" should "admit exactly 352 solutions" in:
    val width             = 5
    val height            = 5
    val numberOfSolutions = 352
    placeMarks(width, height)(width * height).size should be(numberOfSolutions)
