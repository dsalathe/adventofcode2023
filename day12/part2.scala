import io.Source
import scala.collection.mutable.HashMap as HMap

object Part2 {

    def main(args: Array[String]): Unit = 
        val lines = parseInput(args(0))
        println(
            lines.map{case (s, numbers) =>
                comb(s, numbers)    
            }.sum
        )

    def comb(s: String, numbers: Seq[Int]): Long =
        def helper(remaining: Seq[Char], currentCount: Int, remainingNumbers: Seq[Int], cache: HMap[(Seq[Char], Int, Seq[Int]), Long]): Long =
            val res: Long =
            if cache.contains((remaining, currentCount, remainingNumbers)) then
                cache((remaining, currentCount, remainingNumbers))
            else if remaining.isEmpty && remainingNumbers.isEmpty then
                1L
            else if remaining.isEmpty then
                0L
            else
                remaining.head match
                    case '.' =>
                        if currentCount == 0 then
                            helper(remaining.tail, 0, remainingNumbers, cache)
                        else if remainingNumbers.head != currentCount then
                            0L
                        else
                            helper(remaining.tail, 0, remainingNumbers.tail, cache)

                    case '#' => if remainingNumbers.isEmpty then 0L else
                        val newCount = currentCount + 1
                        if newCount > remainingNumbers.head then 0L else helper(remaining.tail, newCount, remainingNumbers, cache)

                    case '?' => helper('#' +: remaining.tail, currentCount, remainingNumbers, cache) + helper('.' +: remaining.tail, currentCount, remainingNumbers, cache)
            cache.put((remaining, currentCount, remainingNumbers), res)
            res

        helper(s.toCharArray.appended('.').toList, 0, numbers.toList, HMap.empty)



    def parseInput(s: String): Seq[(String, Seq[Int])] =
        val lines = Source.fromFile(s).getLines().filter(!_.isBlank())
        lines.map(l => l.split(" ")).map{case Array(inst, numbers) => (inst, numbers.split(",").map(_.toInt).toSeq)}.toSeq
            .map{case (s, numbers) =>
                (Array.fill(5)(s).mkString("?"), Array.fill(5)(numbers).toSeq.flatten)    
            }
}