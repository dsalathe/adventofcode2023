import io.Source

object Part2 {

    def main(args: Array[String]): Unit =
        val (startPosition, graph) = parseInput(args(0))
        val loop = trackLoop(startPosition, graph)
        println(countTiles(loop, graph))

    def countTiles(loop: Set[Position], graph: Graph): Int =
        graph.allPositions
            .filter(position => !loop.contains(position) && isInside(position, loop, graph))
            .size
 
    def isInside(position: Position, loop: Set[Position], graph: Graph): Boolean =
        val wallsOnRight = (position.x until graph.width)
                .map(x => Position(x, position.y))
                .filter(loop.contains)
                .map(graph.getPositionSymbol)
                .filter(_ != '-')
                .mkString
                .replace("F7", "")
                .replace("LJ", "")
                .replace("L7", "|")
                .replace("FJ", "|")

        wallsOnRight.size % 2 == 1

    // We leverage the fact that no node has more than 2 neighbors.
    def trackLoop(startPosition: Position, graph: Graph): Set[Position] =
        def helper(from: Direction, position: Position, path: Set[Position]): Set[Position] =
            // Don't go back, choose the right direction
            val chosenDirection = graph.getPositionDirections(position).filter(_ != from.opposite).head
            val nextPosition = position.move(chosenDirection)
            if nextPosition == startPosition then
                path
            else
                helper(chosenDirection, nextPosition, path + nextPosition)

        // Starts the walk through the loop. It doesn't matter wich first direction we chose.  
        helper(graph.getPositionDirections(startPosition).head.opposite, startPosition, Set(startPosition))


    enum Direction:
        case N, E, S, W
        def opposite = this match
            case N => S
            case E => W
            case S => N
            case W => E

    case class Position(x: Int, y: Int):
        lazy val neighbors = Seq(Position(x, y-1), Position(x+1, y), Position(x, y+1), Position(x-1, y))
        def move(direction: Direction): Position = neighbors(direction.ordinal)
       
    case class Tile(position: Position, symbol: Char):
        export position.*

    case class Node(tile: Tile, directions: Seq[Direction]):
        export tile.*
   
    case class Graph(nodes: Map[Position, Node], width: Int, height: Int):
        val allPositions: Seq[Position] =
            for
                y <- 0 until height
                x <- 0 until width
            yield
                Position(x, y)
 
        def getPositionSymbol(position: Position): Char = nodes(position).symbol
        def getPositionDirections(position: Position): Seq[Direction] = nodes(position).directions
       

    def parseInput(s: String): (Position, Graph) =
        val lines = Source.fromFile(s).getLines().filter(!_.isBlank()).toSeq
        val startPosition: Position = lines.zipWithIndex
            .flatMap{case (line, y) => line.zipWithIndex.map {case (c, x) => (c, x, y)}}
            .find(_._1 == 'S').map(cxy => Position(cxy._2, cxy._3)).get
 
        val nodesWithCoveredStart = lines.zipWithIndex.flatMap {case (line, y) =>
            line.zipWithIndex.map {case (c, x) =>
                val position = Position(x, y)
                val directions = symbolToDirections(c)            
                Node(Tile(position, c), directions)
            }
        }.map(node => node.position -> node).toMap

        val (uncoveredStartSymbol, uncoveredStartDirections) = uncoverStart(nodesWithCoveredStart, startPosition)
        val nodesWithUncoveredStart = nodesWithCoveredStart.updated(startPosition, Node(
                                                        Tile(startPosition, uncoveredStartSymbol),
                                                        uncoveredStartDirections))

        (startPosition, Graph(nodesWithUncoveredStart, lines.head.size, lines.size))

    def symbolToDirections(c: Char): Seq[Direction] =
        import Direction.*
        c match
            case '.' => Seq()
            case '|' => Seq(N, S)
            case '-' => Seq(W, E)
            case 'L' => Seq(N, E)
            case 'J' => Seq(N, W)
            case '7' => Seq(S, W)
            case 'F' => Seq(S, E)
            case 'S' => Seq() // It's unknown with only the character information
            case  _  => throw new AssertionError("should not happen")

    def uncoverStart(nodes: Map[Position, Node], startPosition: Position): (Char, Seq[Direction]) =
        val neighbors = startPosition.neighbors.flatMap(nodes.get)
        val startDirections = (neighbors zip Direction.values.map(_.opposite))
            .filter {case (neighbor, targetDirection) => neighbor.directions.contains(targetDirection)}
            .map{ case (neighbor, direction) => direction.opposite}

        import Direction.*
        val symbol = startDirections.sortBy(_.ordinal) match
            case N +: E +: _ => 'L'
            case N +: S +: _ => '|'
            case N +: W +: _ => 'J'
            case E +: S +: _ => 'F'
            case E +: W +: _ => '-'
            case S +: W +: _ => '7'
            case _ => throw new AssertionError("should not happen")

        (symbol, symbolToDirections(symbol))    
}