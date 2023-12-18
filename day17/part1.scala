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

    // Lazy to help for initialization
    lazy val right = Direction.fromOrdinal((ordinal + 1) % 4)
    lazy val left = Direction.fromOrdinal((ordinal + 3) % 4)

  case class Position(x: Int, y: Int):
    override def toString(): String = s"[$x, $y]"
    def move(direction: Direction): Position = direction match
      case Direction.N => Position(x, y-1)
      case Direction.E => Position(x+1, y)
      case Direction.S => Position(x, y+1)
      case Direction.W => Position(x-1, y)

  case class Node(position: Position, depth: Int, direction: Direction):
    def move(newDirection: Direction): Node = Node(position.move(newDirection), if newDirection == direction then depth + 1 else 1, newDirection)

  def dijkstra(graph: Map[Position, Int], startPosition: Position, goalPosition: Position): Int =
    val startNode = Node(startPosition, 0, Direction.E)
    val gScores = HMap[Node, Int]()
    gScores.put(startNode, 0)
    val pq = PriorityQueue[Node]()(Ordering.by(gScores).reverse)
    pq.enqueue(startNode)
    
    while (!pq.isEmpty) {
      val current = pq.dequeue()
      if current.position == goalPosition then
        return gScores(current)
      else 
        for 
          neighbor <- getNeighbors(current)
        do
          val tentativeGScore = gScores(current) + graph.getOrElse(neighbor.position, Int.MaxValue - gScores(current))
          if tentativeGScore < gScores.getOrElse(neighbor, Int.MaxValue) then
            gScores.put(neighbor, tentativeGScore)
            pq.enqueue(neighbor)
    }
    -1

  def getNeighbors(current: Node): Set[Node] =
    val neighborDirections: Set[Direction] = Set(
          Some(current.direction.right),
          Some(current.direction.left),
          Option.when(current.depth < 3)(current.direction)).flatten
    neighborDirections.map(current.move)
  
  def parseInput(s: String): Map[Position, Int] =
    val digits = Source.fromFile(s).getLines().filter(!_.isBlank()).map(line => line.map(_.asDigit).toVector).toVector
    digits.zipWithIndex.flatMap {case (line, y) =>
      line.zipWithIndex.map {case (heat, x) =>
        Position(x, y) -> heat
      }  
    }.toMap
}