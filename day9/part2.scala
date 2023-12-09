import io.Source

object Part2 {

    def main(args: Array[String]): Unit = 
        val lines = parseInput(args(0))
        println(lines.map(predictValue).sum)

    def predictValue(numbers: Seq[Int]): Int =
        // As before, we also need to compute all differentials.
        // Now the trick is that we can compute the sum A - B + C - D ... where letters are the first diff value of each stage
        val differentials = computeAllDiffs(numbers)
        differentials.map(_.head).zipWithIndex.map {case (v, i) =>
            (1 - 2 * (i%2)) * v // if i is even then it's 1 * v otherwise it's -1 * v
        }.sum

    def computeAllDiffs(numbers: Seq[Int]): Seq[Seq[Int]] =
        def helper(res: Seq[Seq[Int]]): Seq[Seq[Int]] = 
            if res.head.forall(_ == 0) then res.reverse else helper(computeDiffs(res.head) +: res)
        helper(Seq(numbers))
        

    def computeDiffs(numbers: Seq[Int]) = 
        (numbers.tail zip numbers).map {case (n2, n1) => n2 - n1}

    def parseInput(s: String): Seq[Seq[Int]] =
        val lines = Source.fromFile(s).getLines().filter(!_.isBlank())
        lines.map(line => line.split(" ").map(_.toInt).toSeq).toSeq
}