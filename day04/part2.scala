import io.Source

object Part2 {

    def main(args: Array[String]): Unit = 
        val lines = parseInput(args(0))
        val counts = lines.map{case (winningNumbers, ownNumbers) => ownNumbers.count(n => winningNumbers.contains(n))}
        val scratchCardsWon = Array.fill(lines.size)(1)
        counts.zipWithIndex.foreach {case (c, i) => 
            val nCardsAtI = scratchCardsWon(i)
            (i+1 until math.min(i+1+c, scratchCardsWon.size)).foreach(j => scratchCardsWon(j) += nCardsAtI)
        }
        println(
            scratchCardsWon.sum
        )
        


    def parseInput(s: String): Seq[(Set[Int], Seq[Int])] =
        val lines = Source.fromFile(s).getLines().filter(!_.isBlank())
        lines.map{l =>
            val Array(winningNumbers, ownNumbers) = l.split(":")(1).split("\\|").map(_.trim())
            (winningNumbers.split("\\s+").map(_.toInt).toSet, ownNumbers.split("\\s+").map(_.toInt).toSeq)
        }.toSeq
}