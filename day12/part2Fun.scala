import io.Source

object Part2Fun {

    def main(args: Array[String]): Unit = 
        val lines = parseInput(args(0))
        println(
            lines.map{case (s, numbers) =>
                comb(s, numbers)    
            }.sum
        )

    def comb(s: String, numbers: Seq[Int]): Long =
        case class Key(remaining: Seq[Char], count: Int, remainingNumbers: Seq[Int])
        def helper(key: Key, cache: Map[Key, Long]): Map[Key, Long] =
            val Key(remaining, currentCount, remainingNumbers) = key // Unapply
            if cache.contains(key) then
                cache
            else if remaining.isEmpty && remainingNumbers.isEmpty then
                cache + (key -> 1L)
            else if remaining.isEmpty then
                cache + (key -> 0L)
            else
                remaining.head match
                    case '.' =>
                        if currentCount == 0 then
                            val updatedKey = Key(remaining.tail, 0, remainingNumbers)
                            val updatedCache = helper(updatedKey, cache)
                            updatedCache + (key -> updatedCache(updatedKey))
                        else if remainingNumbers.head != currentCount then
                            cache + (key -> 0L)
                        else
                            val updatedKey = Key(remaining.tail, 0, remainingNumbers.tail)
                            val updatedCache = helper(updatedKey, cache)
                            updatedCache + (key -> updatedCache(updatedKey))

                    case '#' => if remainingNumbers.isEmpty then cache + (key -> 0L) else
                        val newCount = currentCount + 1
                        if newCount > remainingNumbers.head then cache + (key -> 0L) else 
                            val updatedKey = Key(remaining.tail, newCount, remainingNumbers)
                            val updatedCache = helper(updatedKey, cache)
                            updatedCache + (key -> updatedCache(updatedKey))


                    case '?' =>
                        val updatedKey1 = Key('#' +: remaining.tail, currentCount, remainingNumbers)
                        val updatedCache1 = helper(updatedKey1, cache)

                        val updatedKey2 = Key('.' +: remaining.tail, currentCount, remainingNumbers)
                        val updatedCache2 = helper(updatedKey2, updatedCache1)
                        updatedCache2 + (key -> (updatedCache2(updatedKey1) + updatedCache2(updatedKey2)))
        
        val key = Key(s.toCharArray.appended('.').toList, 0, numbers.toList)
        helper(key, Map.empty)(key)



    def parseInput(s: String): Seq[(String, Seq[Int])] =
        val lines = Source.fromFile(s).getLines().filter(!_.isBlank())
        lines.map(l => l.split(" ")).map{case Array(inst, numbers) => (inst, numbers.split(",").map(_.toInt).toSeq)}.toSeq
            .map{case (s, numbers) =>
                (Array.fill(5)(s).mkString("?"), Array.fill(5)(numbers).toSeq.flatten)    
            }
}