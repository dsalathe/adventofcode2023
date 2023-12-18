import io.Source

object Part1 {

    def main(args: Array[String]): Unit = 
        val lines = parseInput(args(0))
        println(
            lines.map{case (winningNumbers, ownNumbers) =>
                val c = ownNumbers.count(n => winningNumbers.contains(n))
                if c == 0 then 0 else math.pow(2, c-1).toInt
            }.sum
        )
        


    def parseInput(s: String): Seq[(Set[Int], Seq[Int])] =
        val lines = Source.fromFile(s).getLines().filter(!_.isBlank())
        lines.map{l =>
            val Array(winningNumbers, ownNumbers) = l.split(":")(1).split("\\|").map(_.trim())
            (winningNumbers.split("\\s+").map(_.toInt).toSet, ownNumbers.split("\\s+").map(_.toInt).toSeq)
        }.toSeq
}