import io.Source
import scala.collection.immutable.Queue

object Part1 {

    def main(args: Array[String]): Unit = 
        val (graph, startPosition, endPosition) = parseInput(args(0))
        println(longestBfs(graph, startPosition, endPosition).head.size - 1)
    
    def longestBfs(graph: Map[Position, Char], startPosition: Position, endPosition: Position): List[List[Position]] =
        def helper(paths: List[List[Position]], validPaths: List[List[Position]]): List[List[Position]] = if paths.isEmpty then validPaths else
            val newPaths = paths.flatMap { path =>
                val p = path.head
                val neighbors = List(
                    Option.when(graph.get(p.N).exists(Set('.', '^')))(p.N),
                    Option.when(graph.get(p.E).exists(Set('.', '>')))(p.E),
                    Option.when(graph.get(p.S).exists(Set('.', 'v')))(p.S),
                    Option.when(graph.get(p.W).exists(Set('.', '<')))(p.W)
                ).flatten
                .filterNot(path.contains)
                neighbors.map(path.prepended)
            }
            val newValidPaths = newPaths.filter(path => path.head == endPosition) ++ validPaths
            helper(newPaths, newValidPaths)

        helper(List(List(startPosition)), List(List()))


    case class Position(x: Int, y: Int):
        def N = Position(x, y-1)
        def E = Position(x+1, y)
        def S = Position(x, y+1)
        def W = Position(x-1, y)

    def parseInput(s: String): (Map[Position, Char], Position, Position) =
        val lines = Source.fromFile(s).getLines().filter(!_.isBlank())
        val graph = lines.zipWithIndex.flatMap{case (l, y) => l.zipWithIndex.map {case (c, x) => Position(x, y) -> c}}.toMap
        val startPosition = graph.find((position, c) => position.y == 0 && c == '.').get._1
        val lastLinePosition = graph.map(_._1.y).max
        val endPosition = graph.find((position, c) => position.y == lastLinePosition && c == '.').get._1
        (graph, startPosition, endPosition)
}