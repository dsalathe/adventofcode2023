import io.Source

object Part1 {

    def main(args: Array[String]): Unit = 
        val (seeds, maps) = parseInput(args(0))
        val results = seeds.map(processMaps(_, maps))
        println(results.min)


    case class Filter(value: Long, start: Long, rangeSize: Long)
    type FilterLevel = Seq[Filter]
        

    def processMaps(v: Long, maps: Seq[FilterLevel]): Long = maps match
        case Seq() => v 
        case filter +: tail => 
            val newV = filter.find(f => v >= f.start && v < f.start + f.rangeSize).map(f => f.value - f.start + v).getOrElse(v)
            processMaps(newV, tail)
    


    def parseInput(s: String): (Seq[Long], Seq[FilterLevel]) =
        val lines = Source.fromFile(s).getLines().filter(!_.isBlank()).toSeq
        val rawSeeds = lines.head
        val rawMaps = lines.tail
        val seeds = rawSeeds.split(": ")(1).split(" ").map(_.toLong).toList
        val maps = rawMaps.mkString(",").split("map:")
            .map(s => s.takeWhile(c =>  c.isDigit || c.isSpaceChar || c == ','))
            .map(l => l.split(",").toList.tail.map(trio => trio.split(" ").map(_.toLong).toSeq).map(trio => Filter(trio(0), trio(1), trio(2))))
        (seeds, maps.toSeq)
}