import io.Source

object Part1 {

    def main(args: Array[String]): Unit = 
        val lines = parseInput(args(0))
        println(
            lines.map{case (s, numbers) =>
                countValids(comb(s), numbers)    
            }.sum
        )

    def countValids(possibilities: Seq[String], numbers: Seq[Int]): Long =
        possibilities.count(possibility => isValid(possibility, numbers))

    def isValid(possibility: String, numbers: Seq[Int]): Boolean =
        val groups = possibility.split("\\.").map(_.size).filter(_ != 0)
        groups.toSeq == numbers

    def comb(s: String): Seq[String] =
        // Naively generate all possibilities recursively
        def helper(remaining: Seq[Char], generated: Seq[Seq[Char]]): Seq[Seq[Char]] =
            if remaining.isEmpty then generated else
                remaining.head match 
                    case '?' => helper(remaining.tail, ('.' +: generated.head) +: generated.tail) ++ helper(remaining.tail, ('#' +: generated.head) +: generated.tail)
                    case c => helper(remaining.tail, (c +: generated.head) +: generated.tail)
        helper(s.toSeq, Seq(Seq())).map(_.mkString.reverse)



    def parseInput(s: String): Seq[(String, Seq[Int])] =
        val lines = Source.fromFile(s).getLines().filter(!_.isBlank())
        lines.map(l => l.split(" ")).map{case Array(inst, numbers) => (inst, numbers.split(",").map(_.toInt).toSeq)}.toSeq
}