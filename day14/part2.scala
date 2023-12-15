//> using platform "scala-native"
//> using nativeMode "release-full"
//> using nativeGc "none"

import io.Source
import scala.collection.mutable.ArrayBuffer

object Part2 {

  val N_CYCLES = 1_000_000_000

  def main(args: Array[String]): Unit =
    val platform = parseInput(args(0))
    val (cycleLength, startingLength) = hareAndTortoise(platform)
    val simulationNumber = ((N_CYCLES - startingLength) % cycleLength) + startingLength
    val res = (1 to simulationNumber).foldLeft(platform)((p, _) => p.spin())
    println(res.countWeight())
 

  def hareAndTortoise(start: Platform): (Int, Int) =
    def run(hare: Platform, tortoise: Platform, currentCount: Int=0): (Platform, Int) =
      if hare == tortoise then
        (hare, currentCount)
      else
        run(hare.spin().spin(), tortoise.spin(), currentCount + 1)

    val (firstEncounter, _) = run(start.spin().spin(), start.spin())
    val (secondEncounter, mu) = run(firstEncounter, start)
    val (_, lambda) = run(secondEncounter.spin(), secondEncounter, 1)
    (lambda, mu)


  def parseInput(s: String): Platform =
    Platform(Source.fromFile(s).getLines().filter(!_.isBlank()).map(_.toSeq).toSeq)

  case class Platform(grid: Seq[Seq[Char]]):
    import scala.collection.mutable.ArrayBuffer
    enum Direction:
      case N, W, S, E

    def countWeight(): Int =
      val size = grid.size
      grid.zipWithIndex.map { case (line, i) => line.count(_ == 'O') * (size - i) }.sum

    def spin(): Platform =
      import Direction.*
      val mutableGrid = grid.map(_.to(ArrayBuffer)).to(ArrayBuffer)
      tilt(mutableGrid, N)
      tilt(mutableGrid, W)
      tilt(mutableGrid, S)
      tilt(mutableGrid, E)
      Platform(mutableGrid.map(_.toSeq).toSeq)

    private def tilt(grid: ArrayBuffer[ArrayBuffer[Char]], direction: Direction): Unit =
      for
        _ <- grid.indices
        y <- grid.indices
        x <- grid.head.indices
      do
        direction match
          case Direction.N =>
            if y < grid.head.size - 1 && grid(y)(x) == '.' && grid(y + 1)(x) == 'O' then
              grid(y)(x) = 'O'
              grid(y + 1)(x) = '.'
          case Direction.W =>
            if x < grid.size - 1 && grid(y)(x) == '.' && grid(y)(x + 1) == 'O' then
              grid(y)(x) = 'O'
              grid(y)(x + 1) = '.'
          case Direction.S =>
            if y < grid.head.size - 1 && grid(y)(x) == 'O' && grid(y + 1)(x) == '.' then
              grid(y)(x) = '.'
              grid(y + 1)(x) = 'O'
          case Direction.E =>
            if x < grid.size - 1 && grid(y)(x) == 'O' && grid(y)(x + 1) == '.' then
              grid(y)(x) = '.'
              grid(y)(x + 1) = 'O'

}