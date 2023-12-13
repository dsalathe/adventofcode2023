import io.Source

object Part1 {

    def main(args: Array[String]): Unit = 
        val universe = parseInput(args(0))
        val galaxyCoord = findGalaxiesCoord(universe)
        println(
            (for 
                (coord1, i) <- galaxyCoord.zipWithIndex
                (coord2, j) <- galaxyCoord.zipWithIndex
                if i < j
            yield manhattanDistance(coord1, coord2)).sum
        )


    def manhattanDistance(coord1: (Int, Int), coord2: (Int, Int)): Int =
        math.abs(coord1._1 - coord2._1) + math.abs(coord1._2 - coord2._2)

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


    def parseInput(s: String): Seq[Seq[Char]] =
        val universeWithoutExpansions = Source.fromFile(s).getLines().filter(!_.isBlank()).map(_.toSeq).toSeq
        universeWithoutExpansions.flatMap{line =>
            if line.count(_ == '#') == 0 then Seq(line, line) else Seq(line)    
        }.transpose.flatMap{line =>
            if line.count(_ == '#') == 0 then Seq(line, line) else Seq(line)    
        }.transpose
}