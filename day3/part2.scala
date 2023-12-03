import io.Source

object Part2 {

    type Coord = (Int, Int)

    def main(args: Array[String]): Unit =
        val engineMap = EngineMap.createMap(args(0))
        val engineCoords = engineMap.findCoords(isGear)
        val startNumbers = engineCoords.map(engineMap.findAllStartNumbers).filter(_.size == 2)
        val numbers = startNumbers.map(s => s.map(engineMap.coordToNumber))
        println(numbers.map(s => s.reduce(_ * _)).sum)


    def isGear(c: Char): Boolean = c == '*'

    object EngineMap:
        def createMap(fn: String): EngineMap =
            EngineMap(Source.fromFile(fn).getLines().filter(!_.isBlank()).map(_.toVector).toVector)

    case class EngineMap(engineMap: Vector[Vector[Char]]):

        def findCoords(cond: Char => Boolean): Vector[Coord] =
            engineMap.zipWithIndex.flatMap{case (line, y) => line.zipWithIndex.map{case (symbol, x) => (symbol, (x, y))}}.filter{case (symbol, coord) => cond(symbol)}.map(_._2)

        def findAllStartNumbers(coord: Coord): Set[Coord] =
            val neighbors = findNeighbors(coord)
            neighbors.map(findStartNumber)

        def findNeighbors(coord: Coord): Set[(Coord)] =
            (for x <- (coord._1 - 1) to (coord._1 + 1)
                y <- (coord._2 - 1) to (coord._2 + 1)
                if x >= 0 && x < engineMap(0).length && y >= 0 && y < engineMap.length && engineMap(y)(x).isDigit 
            yield (x, y)).toSet

        def findStartNumber(coord: Coord): Coord =
            val (x, y) = coord
            if x == 0 then coord else if !engineMap(y)(x-1).isDigit then coord else findStartNumber((x-1, y))

        def coordToNumber(coord: Coord): Int =
            val (x, y) = coord
            engineMap(y).drop(x).takeWhile(_.isDigit).mkString.toInt

}

