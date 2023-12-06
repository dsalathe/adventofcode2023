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
object Part2 {

    case class Record(time: Long, distance: Long)

    def main(args: Array[String]): Unit = 
        val record = parseInput(args(0))
        val x = solveX(record.time, record.distance)
        println(computeRange(record.time, x))


    def computeRange(n: Long, x: Long) = n - 2*x + 1

    def solveX(n: Long, c: Long): Long =
        val delta: Long = n*n - 4 * c
        val res = (n - math.sqrt(delta)) / 2
        math.floor(res).toLong + 1

    def parseInput(fn: String): Record =
        val lines = Source.fromFile(fn).getLines().filter(!_.isBlank()).map(_.filter(_.isDigit).toLong).toSeq
        Record(lines(0), lines(1))
        
}