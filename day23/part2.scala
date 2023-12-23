import io.Source
import scala.collection.mutable.Set as HSet

object Part2 {

    def main(args: Array[String]): Unit = 
        val (graph, startPosition, endPosition) = parseInput(args(0))
        val compressedGraph = compress(graph, startPosition)
        println(longestPath(compressedGraph, startPosition, endPosition))


    def longestPath(graph: Map[Position, Set[Edge]], startPosition: Position, endPosition: Position): Int =
        def traverse(paths: List[List[Position]], longest: Int): Int = if paths.isEmpty then longest else
            val newPaths = paths.flatMap(path =>
                graph(path.head).map(e => e.to).filterNot(path.contains).map(p => p +: path)    
            )
            val newLongest = newPaths.filter(path => path.head == endPosition).map(computeSize(graph, _)).maxOption.getOrElse(0)
            traverse(newPaths, math.max(longest, newLongest))
        traverse(List(List(startPosition)), 0)

    def computeSize(graph: Map[Position, Set[Edge]], path: List[Position]): Int =
        (path zip path.tail).map { case (p1, p2) => graph(p1).toSeq.filter(e => e.to == p2).head.distance}.sum

    def compress(graph: Map[Position, Char], startPosition: Position): Map[Position, Set[Edge]] =
        def findNextJunction(traversed: List[Position]): List[Position] =
            val p = traversed.head
            val neighbors = findNeighbors(graph, p).filterNot(traversed.contains)
            if neighbors.size == 1 then findNextJunction(neighbors.head +: traversed) else traversed

        def buildEdges(p: Position, edges: HSet[Edge]): Unit =
            val neighbors = findNeighbors(graph, p)
            val newEdges = neighbors
                            .map(n => findNextJunction(List(n, p)))
                            .map(path => Edge(p, path.head, path.size-1)).filterNot(edges.contains)
            edges.addAll(newEdges)
            newEdges.foreach(edge => buildEdges(edge.to, edges))

        val mutableEdges = HSet[Edge]()
        buildEdges(startPosition, mutableEdges)
        val edges = mutableEdges.toSet
        graph.keySet.map(p => p -> edges.filter(e => e.from == p)).toMap
            
    def findNeighbors(graph: Map[Position, Char], p: Position): Set[Position] =
        Set(
            Option.when(graph.get(p.N).exists(_ != '#'))(p.N),
            Option.when(graph.get(p.E).exists(_ != '#'))(p.E),
            Option.when(graph.get(p.S).exists(_ != '#'))(p.S),
            Option.when(graph.get(p.W).exists(_ != '#'))(p.W)
        ).flatten

      
    case class Edge(from: Position, to: Position, distance: Int)

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