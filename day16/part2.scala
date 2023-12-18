import io.Source

object Part2 {

  def main(args: Array[String]): Unit =
    val grid = parseInput(args(0))
    val maxX = grid.map(_._1.x).max
    val maxY = grid.map(_._1.y).max
    val countFunction = countEnergizedTiles(grid)
    val fromLeft = (0 until maxY).map(y => countFunction(Position(0, y), Direction.E))
    val fromTop = (0 until maxX).map(x => countFunction(Position(x, 0), Direction.S))
    val fromRight = (0 until maxY).map(y => countFunction(Position(maxX-1, y), Direction.W))
    val fromBottom = (0 until maxX).map(x => countFunction(Position(x, maxY-1), Direction.N))
    println((fromLeft ++ fromTop ++ fromRight ++ fromBottom).max)

  enum Direction:
    case N, E, S, W

  case class Position(x: Int, y: Int):
    def move(direction: Direction): Position = direction match
      case Direction.N => Position(x, y-1)
      case Direction.E => Position(x+1, y)
      case Direction.S => Position(x, y+1)
      case Direction.W => Position(x-1, y)

  case class LightParticle(position: Position, direction: Direction):
    import Direction.*
    def move(c: Char): Seq[LightParticle] = c match
      case '.' => Seq(moveForward())
      case '/' => 
        val newDirection = direction match
          case N => E
          case E => N
          case S => W
          case W => S
        Seq(move(newDirection))

      case '\\' =>
        val newDirection = direction match
          case N => W
          case E => S
          case S => E
          case W => N
        Seq(move(newDirection))

      case '|' => direction match
        case W | E => Seq(move(Direction.N), move(Direction.S))
        case N | S => Seq(moveForward())

      case '-' => direction match
        case N | S => Seq(move(Direction.W), move(Direction.E))
        case W | E => Seq(moveForward())
        
    
    private def move(direction: Direction): LightParticle = LightParticle(position.move(direction), direction) 
    private def moveForward(): LightParticle = move(direction)
    
  def countEnergizedTiles(grid: Map[Position, Char])(startPosition: Position, startDirection: Direction): Int = 
    traverse(grid, LightParticle(startPosition, startDirection)).map(_._1).size

  def traverse(grid: Map[Position, Char], lightParticle: LightParticle, visited: Set[LightParticle]=Set()): Set[LightParticle] = 
    if !grid.contains(lightParticle.position) || visited.contains(lightParticle) then
      visited
    else
      val updatedVisited = visited + lightParticle
      val particles: Seq[LightParticle] = lightParticle.move(grid(lightParticle.position))
      val traversed = traverse(grid, particles.head, updatedVisited)
      if particles.size == 2 then traverse(grid, particles(1), traversed) else traversed
      // I prefer the version below but it stack-overflows...
      //lightParticle.move(grid(lightParticle.position)).foldLeft(updatedVisited)((visited, lp) => traverse(grid, lp, visited))  


  def parseInput(s: String): Map[Position, Char] =
    val lines = Source.fromFile(s).getLines().filter(!_.isBlank()).map(_.toVector).toVector
    lines.zipWithIndex.flatMap {case (line, y) =>
      line.zipWithIndex.map {case (c, x) =>
        Position(x, y) -> c
      }  
    }.toMap


}