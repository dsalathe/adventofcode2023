//> using platform "scala-native"
//> using nativeMode "release-full"
//> using nativeGc "none"

import io.Source

object Part2Fun {

  val N_CYCLES = 1_000_000_000

  def main(args: Array[String]): Unit =
    val platform = parseInput(args(0))
    val (cycleLength, startingLength) = hareAndTortoise(platform)
    println(s"$cycleLength $startingLength")
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
    Platform(Source.fromFile(s).getLines().filter(!_.isBlank()).map(_.toVector).toVector)

  enum Direction:
      case N, W, S, E
  
  case class Platform(grid: Vector[Vector[Char]]):
    import scala.collection.mutable.ArrayBuffer

    def countWeight(): Int =
      val size = grid.size
      grid.zipWithIndex.map { case (line, i) => line.count(_ == 'O') * (size - i) }.sum

    def spin(): Platform =
      val mutableGrid = grid.map(_.to(ArrayBuffer)).to(ArrayBuffer)
      Direction.values.foldLeft(this)((p, d) => p.tilt(d))

    def tilt(direction: Direction): Platform =
      val transposedGrid = if direction == Direction.N || direction == Direction.S then grid.transpose else grid
      val newGrid = transposedGrid.map { line =>
          val groups = line.mkString.split("#", -1)
          groups.map { group => 
            val nRocks = group.count(_ == 'O')
            val nSpaces = group.count(_ == '.')
            if direction == Direction.E || direction == Direction.S then "." * nSpaces + "O" * nRocks else "O" * nRocks + "." * nSpaces
          }.mkString("#").toVector
      }.toVector
      val transposedNewGrid = if direction == Direction.N || direction == Direction.S then newGrid.transpose else newGrid
      Platform(transposedNewGrid)
      
    override def toString(): String = grid.map(_.mkString).mkString("\n")

}