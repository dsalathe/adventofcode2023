import io.Source
import scala.collection.immutable.Queue

object Part2Failed {

    /**
      * Part of a solution which aims to be a general one. For now it is bugged, but the idea is the following:
        rather than expanding indefinitively, we count how many times we discover a relative tile (meaning Position(0,0) is relatively the same as Position(131, 131)).
        We then extrapolate the four series with their respective periodicity for the given tile.
        Finally we compute how many times the tile was reached before the 26_501_365 steps.
      */

    val counters: scala.collection.mutable.HashMap[Position, Int] = scala.collection.mutable.HashMap()

    def main(args: Array[String]): Unit = 
        val (graph, startPosition) = parseInput(args(0))
        val width = graph.map(pc => pc._1.x).max + 1
        val height = graph.map(pc => pc._1.y).max + 1
        val q = Queue[(Position, Int)]().enqueue((startPosition, 0))
        val tilesCount = bfs(graph, width, height, q, Set(), args(1).toInt, Map(startPosition -> Seq(0)))
        val n = args(1).toInt
        val parity = n % 2
        println(s"${tilesCount(Position(0, 3))} n $n parity $parity")
        println(countInTile(tilesCount(Position(0,3)), n, parity))
        //println(tilesCount.values.map(counts => countInTile(counts, n, parity)).sum)
        //println(counters)
        //println(tilesCount.map{case (pos, seq) => (pos, countInTile(seq, n, parity))}.toMap)

    def countInTile(seq: Seq[Int], n: Long, parity: Int): Long =
        val periodicity = seq(0) - seq(4)
        seq.tail.map(c => countOne(c, periodicity, n, parity)).sum
    
    def countOne(a: Int, periodicity: Int, n: Long, parity: Int): Long =
        println(s"countOne a $a periodicity $periodicity n $n")
        val nPassages = (n - a) / periodicity + 1L
        println("nPassages " + nPassages)
        (1L to nPassages).map {p =>
            val result = a + (p-1)*periodicity
            println(result)
            if result % 2 == parity then p else 0    
        }.sum

    def bfs(graph: Map[Position, Char], width: Int, height: Int, q: Queue[(Position, Int)],
            visited: Set[Position], maxDepth: Int, tilesCount: Map[Position, Seq[Int]]): Map[Position, Seq[Int]] =
        if q.isEmpty || tilesCount.values.forall(_.size == 17) then
            println(q.size)
            println("bye")
            tilesCount
        else
            val ((position, depth), remainingQ) = q.dequeue
            if visited.contains(position) || depth > maxDepth then 
                bfs(graph, width, height, remainingQ, visited, maxDepth, tilesCount) 
            else
                val neighbors = position.getNeighbors()
                    .filter(p => graph(Position(positiveModulo(p.x, width), positiveModulo(p.y, height))) != '#')
                    .map(p => (p, depth+1))
                //if depth % 2 == maxDepth % 2 then println(position) 
                val relativePosition = Position(positiveModulo(position.x, width), positiveModulo(position.y, height))
                val currentSize = tilesCount.getOrElse(relativePosition, Seq()).size
                val newTilesCount = 
                    if currentSize < 17 then 
                        tilesCount.updated(relativePosition, depth +: tilesCount.getOrElse(relativePosition, Seq())) 
                    else 
                        tilesCount
                counters.put(relativePosition, counters.getOrElse(relativePosition, 0) + 1)
                bfs(graph, width, height, remainingQ.enqueueAll(neighbors), visited + position, maxDepth, newTilesCount)

    def positiveModulo(a: Int, b: Int) = ((a % b) + b) % b

    case class Position(x: Int, y: Int):
        def getNeighbors() = Seq(Position(x, y-1), Position(x+1, y), Position(x, y+1), Position(x-1, y))

    def parseInput(s: String): (Map[Position, Char], Position) =
        val lines = Source.fromFile(s).getLines().filter(!_.isBlank())
        val graph = lines.zipWithIndex.flatMap {case (line, y) => line.zipWithIndex.map {case (c, x) => Position(x, y) -> c}}.toMap
        val startPosition = graph.filter{case (position, c) => c == 'S'}.map(_._1).head
        (graph, startPosition)
}