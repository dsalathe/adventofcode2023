import io.Source

object Part2Fun {
  val N_CYCLES = 1_000_000_000

  def main(args: Array[String]): Unit =
    val platform = parseInput(args(0))
    val (cycleLength, startingLength) = hareAndTortoise(platform)
    // the final position is the starting path + the remaining path within the loop
    val simulationNumber = startingLength + ((N_CYCLES - startingLength) % cycleLength)
    val res = (1 to simulationNumber).foldLeft(platform)((p, _) => p.spin())
    println(res.countWeight())
 
  def hareAndTortoise(start: Platform): (Int, Int) =
    def run(hare: Platform, tortoise: Platform, currentCount: Int=0): (Platform, Int) =
      if hare == tortoise then
        (hare, currentCount)
      else
        run(hare.spin().spin(), tortoise.spin(), currentCount + 1)

    val (firstEncounter, _) = run(hare = start.spin().spin(), tortoise = start.spin())
    val (secondEncounter, mu) = run(hare = firstEncounter, tortoise = start)
    val (_, lambda) = run(hare = secondEncounter.spin(), tortoise = secondEncounter, currentCount = 1)
    (lambda, mu)

  def parseInput(s: String): Platform =
    Platform(Source.fromFile(s).getLines().filter(!_.isBlank()).map(_.toVector).toVector)

  enum Direction:
      case N, W, S, E
  
  case class Platform(grid: Vector[Vector[Char]]):
    def countWeight(): Int = grid.zipWithIndex.map { case (line, i) => line.count(_ == 'O') * (grid.size - i) }.sum

    def spin(): Platform = Direction.values.foldLeft(this)((p, d) => p.tilt(d))

    def tilt(direction: Direction): Platform =
      import Direction.*
      val transposedGrid = if Set(N, S).contains(direction) then grid.transpose else grid
      val newGrid = transposedGrid.map { line =>
          val groups = line.mkString.split("#", -1) // "-1" allows to produce empty groups. It helps us adding "#" back at the end
          groups.map { group => 
            val rocks = "O" * group.count(_ == 'O')
            val spaces = "." * group.count(_ == '.')
            if Set(E, S).contains(direction) then spaces + rocks else rocks + spaces
          }.mkString("#").toVector
      }.toVector
      val transposedNewGrid = if Set(N, S).contains(direction) then newGrid.transpose else newGrid
      Platform(transposedNewGrid)

}