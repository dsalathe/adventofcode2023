import io.Source
import scala.collection.immutable.Queue

object Part1 {

    def main(args: Array[String]): Unit = 
        val (graph, startPosition) = parseInput(args(0))
        println(bfs(graph, startPosition, 64))

    def bfs(graph: Map[Position, Char], startPosition: Position, maxDepth: Int): Int =
        def helper(q: Queue[(Position, Int)], visited: Set[Position], count: Int): Int =
            if q.isEmpty then
                count
            else
                val ((position, depth), remainingQ) = q.dequeue
                if visited.contains(position) || depth > maxDepth then 
                    helper(remainingQ, visited, count) 
                else
                    val neighbors = position.getNeighbors()
                        .filter(graph.contains)
                        .filter(p => graph(p) != '#')
                        .map(p => (p, depth+1))
                    helper(remainingQ.enqueueAll(neighbors), visited + position, if depth % 2 == maxDepth % 2 then count + 1 else count)
        helper(Queue[(Position, Int)]().enqueue((startPosition, 0)), Set(), 0)

    case class Position(x: Int, y: Int):
        def getNeighbors() = Seq(Position(x, y-1), Position(x+1, y), Position(x, y+1), Position(x-1, y))

    def parseInput(s: String): (Map[Position, Char], Position) =
        val lines = Source.fromFile(s).getLines().filter(!_.isBlank())
        val graph = lines.zipWithIndex.flatMap {case (line, y) => line.zipWithIndex.map {case (c, x) => Position(x, y) -> c}}.toMap
        val startPosition = graph.filter{case (position, c) => c == 'S'}.map(_._1).head
        (graph, startPosition)
}