import io.Source

object Part2 {

    def main(args: Array[String]): Unit = 
        val lines = parseInput(args(0))
        println(lines.map(predictValue).sum)

    def predictValue(numbers: Seq[Int]): Int =
        val differentials = computeAllDiffs(numbers)
        differentials.map(_.head).foldLeft(0)((acc, n) => n - acc)

    def computeAllDiffs(numbers: Seq[Int]): Seq[Seq[Int]] =
        def helper(res: Seq[Seq[Int]]): Seq[Seq[Int]] = 
            if res.head.forall(_ == 0) then res else helper(computeDiffs(res.head) +: res)
        helper(Seq(numbers))
        

    def computeDiffs(numbers: Seq[Int]) = 
        (numbers.tail zip numbers).map {case (n2, n1) => n2 - n1}

    def parseInput(s: String): Seq[Seq[Int]] =
        val lines = Source.fromFile(s).getLines().filter(!_.isBlank())
        lines.map(line => line.split(" ").map(_.toInt).toSeq).toSeq
}