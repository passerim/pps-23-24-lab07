package ex3

import scala.math.abs

object Solitaire:
  type Position        = (Int, Int)
  type Solution        = Iterable[Position]
  type IterableFactory = Solution => Iterable[Solution]
  given IterableFactory = LazyList(_)
  val width             = 5
  val height            = 5

  def placeMarks(width: Int = width, height: Int = height)(n: Int = width * height)(using
      factory: IterableFactory
  ): Iterable[Solution] =
    n match
      case 1 => factory(Seq((width / 2, height / 2)))
      case n if n > 1 =>
        for
          solution <- placeMarks(width, height)(n - 1)
          x        <- 0 until width
          y        <- 0 until height
          position = (x, y)
          if mark(position, solution)
        yield solution.toSeq :+ position
      case _ => factory(Seq())

  def mark(position: Position, solution: Solution): Boolean =
    (abs(position._1 - solution.last._1), abs(position._2 - solution.last._2)) match
      case (dx, dy)
          if ((dx == 3 && dy == 0) || (dx == 0 && dy == 3) || (dx == 2 && dy == 2)) && !(solution exists (_ == position)) =>
        true
      case _ => false

  def render(solution: Seq[(Int, Int)], width: Int = width, height: Int = height): String =
    val reversed = solution.reverse
    val rows =
      for
        y <- 0 until height
        row = for
          x <- 0 until width
          number = reversed.indexOf((x, y)) + 1
        yield if number > 0 then "%-2d ".format(number) else "X  "
      yield row.mkString
    rows.mkString("\n")

  @main def run(): Unit =
    placeMarks()().zipWithIndex.foreach((s, i) => println(s"Solution $i:\n\n${render(s.toSeq)}\n"))
