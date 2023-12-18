import io.Source

/**
 * EXPLANATION:
    The key is to notice that holding the boat x seconds and releasing it y seconds yields x * y meters.
    we know that:
        x + y = n, where n is the total time
        x * y = z, where z is the result in meters
    By substitution:
        n - x = y
        x * (n - x) = -x² + nx = z
    The rule is to have z > c, where c is the record distance.
        -x² + nx > c
        -x² + nx - c > 0
    This becomes a second degree linear equation, with coefficients *a* = -1, *b* = n and *c* = -c
 **/
object Part1 {

    case class Record(time: Long, distance: Long)

    def main(args: Array[String]): Unit = 
        val lines = parseInput(args(0))
        println(
            lines
                .map(rec => (rec.time, solveX(rec.time, rec.distance)))
                .map(computeRange)
                .reduce(_ * _))


    def computeRange(n: Long, x: Long) = n - 2*x + 1

    def solveX(n: Long, c: Long): Long =
        val delta: Long = n*n - 4 * c
        val res = (n - math.sqrt(delta)) / 2
        math.floor(res).toLong + 1



    def parseInput(fn: String): Seq[Record] =
        val lines = Source.fromFile(fn).getLines().filter(!_.isBlank()).map(_.split(":")(1)).map(_.trim()).toSeq
        val times = lines(0).split("\\s+").map(_.toInt)
        val distances = lines(1).split("\\s+").map(_.toInt)
        (times zip distances map(t => Record(t._1, t._2))).toSeq
        
}