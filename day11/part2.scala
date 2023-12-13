import io.Source

object Part2 {

    val EXPANDED_DISTANCE_ADDED = 1_000_000L - 1

    def main(args: Array[String]): Unit = 
        val (universe, expandedRows, expandedCols) = parseInput(args(0))
        val galaxyCoord = findGalaxiesCoord(universe)
        println(
            (for 
                (coord1, i) <- galaxyCoord.zipWithIndex
                (coord2, j) <- galaxyCoord.zipWithIndex
                if i < j
            yield manhattanDistance(coord1, coord2) + withExpandedDistance(coord1, coord2, expandedRows, expandedCols)).sum
        )

    def withExpandedDistance(coord1: (Int, Int), coord2: (Int, Int), r: Set[Int], c: Set[Int]): Long =
        val sortedByX = Seq(coord1._1, coord2._1).sorted
        val addX = (sortedByX.head to sortedByX.last).count(x => c.contains(x)) * EXPANDED_DISTANCE_ADDED
        val sortedByY = Seq(coord1._2, coord2._2).sorted
        val addY = (sortedByY.head to sortedByY.last).count(y => r.contains(y)) * EXPANDED_DISTANCE_ADDED
        addX + addY

    def manhattanDistance(coord1: (Int, Int), coord2: (Int, Int)): Long =
        (math.abs(coord1._1 - coord2._1) + math.abs(coord1._2 - coord2._2)).toLong

    def findGalaxiesCoord(universe: Seq[Seq[Char]]): Seq[(Int, Int)] =
        universe.zipWithIndex.flatMap {case (line, y) =>
            line.zipWithIndex.map{case (c, x) =>
                (c, (x, y))
            }    
        }.filter {case (c, coord) =>
            c == '#'    
        }.map { case (c, coord) =>
            coord
        }


    def parseInput(s: String): (Seq[Seq[Char]], Set[Int], Set[Int]) =
        val universeWithoutExpansions = Source.fromFile(s).getLines().filter(!_.isBlank()).map(_.toSeq).toSeq
        val expandedRows = findExpandedLines(universeWithoutExpansions)
        val expandedCols = findExpandedLines(universeWithoutExpansions.transpose)
        (universeWithoutExpansions, expandedRows, expandedCols)

    def findExpandedLines(universe: Seq[Seq[Char]]): Set[Int] =
        universe
            .zipWithIndex
            .filter {case (line, y) => line.count(_ == '#') == 0}
            .map(_._2).toSet
}