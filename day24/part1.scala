import io.Source

object Part1 {

    def main(args: Array[String]): Unit = 
        val hailstones = parseInput(args(0))
        println(
            (for 
                h1 <- hailstones
                h2 <- hailstones
                if willCross(h1, h2)
            yield (h1, h2)).size / 2
        )


    def willCross(h1: HailStone, h2: HailStone): Boolean =
        if h1.a == h2.a then
            false
        else
            val x = (h2.b - h1.b) / (h1.a - h2.a)
            val y = h1.a * x + h1.b
            (x - h1.x) / h1.dx > 0
            && (x - h2.x) / h2.dx > 0
            && x >= 200000000000000L && x <= 400000000000000L
            && y >= 200000000000000L && y <= 400000000000000L


    case class HailStone(x: Long, y: Long, z: Long, dx: Long, dy: Long, dz: Long):
        def a = dy.toDouble / dx
        def b = y - a * x

    def parseInput(s: String): Seq[HailStone] =
        val lines = Source.fromFile(s).getLines().filter(!_.isBlank())
        lines.map(line => 
            val values = line.split(" @ ")
            .flatMap(a => a.split(", "))
            .map(_.trim)
            .map(_.toLong)
            values match
                case Array(x,y,z,dx,dy,dz) => HailStone(x,y,z,dx,dy,dz)
                case _ => throw AssertionError()    
            ).toSeq
}