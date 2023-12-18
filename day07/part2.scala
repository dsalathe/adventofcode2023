import io.Source

object Part2 {

    val CARDS_STRENGTH: Map[Char, Int] = "J23456789TQKA".zipWithIndex.toMap

    case class CamelHand(kind: Int, hand: String, bid: Long):
        def toCardinalRepresentation: String = hand.map(CARDS_STRENGTH).map("%02d".format(_)).mkString

    
    given CamelOrdering: Ordering[CamelHand] = Ordering.by {
       camelHand =>
        (camelHand.kind, camelHand.toCardinalRepresentation)
    }

    def main(args: Array[String]): Unit = 
        val lines = parseInput(args(0))
        println(lines.sorted.zipWithIndex.map{
            case (camelHand, i) => camelHand.bid * (i+1)
        }.sum)


    def computeKind(s: String): Int =
        val withoutJ = s.filter(_ != 'J')
        val jCount = s.size - withoutJ.size
        val counts = withoutJ.groupBy(identity).mapValues(_.size).values.toSeq.sorted.reverse
        val adjustedCounts = if jCount == 5 then counts else (counts.head + jCount) +: counts.tail
        adjustedCounts.reverse match
            case Seq(5) => 6
            case Seq(1, 4) => 5
            case Seq(2, 3) => 4
            case Seq(1, 1, 3) => 3
            case Seq(1, 2, 2) => 2
            case Seq(1, 1, 1, 2) => 1
            case _ => 0

        

    def parseInput(s: String): Seq[CamelHand] =
        val lines = Source.fromFile(s).getLines().filter(!_.isBlank())
        lines.map(l => 
            val pair = l.trim().split(" ").toSeq
            CamelHand(computeKind(pair(0)), pair(0), pair(1).toLong)
            ).toSeq
}