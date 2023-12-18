import io.Source
import scala.annotation.tailrec

object Part2 {


  // =============== MAIN ====================
  def main(args: Array[String]): Unit =
    val (seeds, rawMaps): (Seq[Range], Seq[FilterLevel]) = parseInput(args(0))
    val maps: Seq[FilterLevel] = cleanMaps(rawMaps)
    println(processAllMaps(seeds, maps).head.start)
  // =========================================


  // ============== CLASSES ==================
  case class Range(start: Long, end: Long):
    def shift(value: Long): Range = Range(start + value, end + value)
    def intersect(otherRange: Range): Option[Range] =
      val (lowRange, highRange) = if start <= otherRange.start then (this, otherRange) else (otherRange, this)
      Option.when(lowRange.end > highRange.start)(Range(highRange.start, math.min(lowRange.end, highRange.end)))

  case class Filter(value: Long, range: Range):
    export range.*

  type FilterLevel = Seq[Filter]
  // =========================================

  // =============== LOGIC ===================
  @tailrec
  def processAllMaps(seeds: Seq[Range], allMaps: Seq[FilterLevel]): Seq[Range] = allMaps match
    case Seq() => seeds
    case m +: remainingMaps =>
      val rawRanges = seeds.flatMap(seed => processMaps(seed, m))
      val cleanedResults = cleanRanges(rawRanges)
      processAllMaps(cleanedResults, remainingMaps)


  def processMaps(range: Range, filterLevel: FilterLevel): Seq[Range] =
    filterLevel.flatMap(filter => range.intersect(filter.range).map(_.shift(filter.value)))


  def cleanMaps(maps: Seq[FilterLevel]): Seq[FilterLevel] = maps.map { m =>
    if m.isEmpty then
      Seq(Filter(0L, Range(0L, Long.MaxValue)))
    else
      val sortedFilters = m.sortBy(_.start) // This compile thanks to the export clause!
      val firstFilter = Filter(0L, Range(0L, sortedFilters.head.start))
      val lastFilter = Filter(0L, Range(sortedFilters.last.end, Long.MaxValue))
      if sortedFilters.head.start == 0L
      then sortedFilters :+ lastFilter
      else
        firstFilter +: sortedFilters :+ lastFilter
  }

  def cleanRanges(ranges: Seq[Range]): Seq[Range] =
    def cleanRangesHelper(ranges: Seq[Range]): Seq[Range] =
      val sortedRanges = ranges.sortBy(_.start)
      sortedRanges.foldLeft(Seq(sortedRanges.head)) { (ranges, newRange) =>
        val currentRange = ranges.head
        if currentRange.end >= newRange.start then Range(currentRange.start, newRange.end) +: ranges.tail else newRange +: ranges
      }

    cleanRangesHelper(ranges).reverse


  def parseInput(s: String): (Seq[Range], Seq[FilterLevel]) =
    val lines = Source.fromFile(s).getLines().filter(!_.isBlank()).toSeq
    val rawSeeds = lines.head
    val rawMaps = lines.tail
    val seedsStart = rawSeeds.split(": ")(1).split(" ").map(_.toLong).zipWithIndex.filter(_._2 % 2 == 0).map(_._1).toList
    val seedsRange = rawSeeds.split(": ")(1).split(" ").map(_.toLong).zipWithIndex.filter(_._2 % 2 == 1).map(_._1).toList
    val seeds = (seedsStart zip seedsRange).map { case (startRange, sizeRange) => Range(startRange, startRange + sizeRange) }
    val maps = rawMaps.mkString(",")
      .split("map:")
      .map(s => s.takeWhile(c => c.isDigit || c.isSpaceChar || c == ','))
      .map(l => l.split(",").toList.tail.map(trio => trio.split(" ").map(_.toLong).toList).map {
        case Seq(newStartRange, startRange, sizeRange) => Filter(newStartRange - startRange, Range(startRange, startRange + sizeRange))
        case _ => throw new IllegalArgumentException()
      })
    (seeds, maps.toSeq)
  // =========================================
}
