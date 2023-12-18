import io.Source
import scala.collection.mutable.PriorityQueue
import scala.collection.mutable.HashMap as HMap

object Part1 {

  def main(args: Array[String]): Unit =
    val graph = parseInput(args(0))
    val maxX = graph.map(_._1.x).max
    val maxY = graph.map(_._1.y).max
    println(dijkstra(graph, Position(0, 0), Position(maxX, maxY)))

  enum Direction:
    case N, E, S, W  

  case class Position(x: Int, y: Int):
    override def toString(): String = s"[$x, $y]"
    def move(direction: Direction): Position = direction match
      case Direction.N => Position(x, y-1)
      case Direction.E => Position(x+1, y)
      case Direction.S => Position(x, y+1)
      case Direction.W => Position(x-1, y)

  case class Node(position: Position, depth: Int, direction: Direction)

  def dijkstra(graph: Map[Position, Int], startPosition: Position, goalPosition: Position): Int =
    import Direction.*

    val startNode = Node(startPosition, 0, E)
    val gScores = HMap[Node, Int]()
    gScores.put(startNode, 0)
    val pq = PriorityQueue[Node]()(Ordering.by(gScores).reverse)
    pq.enqueue(startNode)    
    
    while (!pq.isEmpty) {
      val current = pq.dequeue()
      if current.position == goalPosition then
        return gScores(current)
      else 
        val neighbors: Set[Direction] = Set(
          Some(Direction.fromOrdinal((current.direction.ordinal+1) % 4)), // Turn right
          Some(Direction.fromOrdinal((current.direction.ordinal+3) % 4)),  // Turn left
          Option.when(current.depth < 3)(current.direction)).flatten // Move forward
        for 
          neighborDirection <- neighbors
        do
          val neighbor = current.position.move(neighborDirection)
          val neighborDepth = if neighborDirection == current.direction then current.depth + 1 else 1
          val neighborNode = Node(neighbor, neighborDepth, neighborDirection)
          val tentativeGScore = gScores(current) + graph.getOrElse(neighbor, Int.MaxValue - gScores(current))
          if tentativeGScore < gScores.getOrElse(neighborNode, Int.MaxValue) then
            gScores.put(neighborNode, tentativeGScore)
            pq.enqueue(neighborNode)

    }
    -1
  
  def parseInput(s: String): Map[Position, Int] =
    val digits = Source.fromFile(s).getLines().filter(!_.isBlank()).map(line => line.map(_.asDigit).toVector).toVector
    digits.zipWithIndex.flatMap {case (line, y) =>
      line.zipWithIndex.map {case (heat, x) =>
        Position(x, y) -> heat
      }  
    }.toMap
}