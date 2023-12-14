import io.Source

object Part1 {

    def main(args: Array[String]): Unit = 
        val lines = parseInput(args(0))
        println(lines.map(pattern => getReflection(pattern)).sum)
        
    def getReflection(pattern: Seq[Seq[Char]]): Int = 
        findReflection(pattern.toVector).map(_ * 100).orElse(findReflection(pattern.transpose.toVector)).get


    def findReflection(pattern: Vector[Seq[Char]]): Option[Int] =
        (1 until pattern.length).find {i =>
            val (left, right) = pattern.splitAt(i)
            left.reverse.take(right.size) == right.take(left.size)
        }


    def parseInput(s: String): Seq[Seq[Seq[Char]]] =
        Source.fromFile(s).getLines().mkString("\n").split("\n\n").toSeq.map(pattern => pattern.split("\n").toSeq.map(_.toCharArray().toSeq))
}