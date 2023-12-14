import io.Source

object Part2 {

    def main(args: Array[String]): Unit = 
        val lines = parseInput(args(0))
        println(lines.map(pattern => getReflection(pattern)).sum)
        
    def getReflection(pattern: Seq[Seq[Char]]): Int = 
        findReflectionWithSmudge(pattern.toVector).map(_ * 100).orElse(findReflectionWithSmudge(pattern.transpose.toVector)).get

    def findReflectionWithSmudge(pattern: Vector[Seq[Char]]): Option[Int] =
        val reflection = findReflections(pattern).headOption
        val updateds =
            for 
                (l, i) <- pattern.zipWithIndex
                (c, j) <- l.zipWithIndex
            yield pattern.updated(i, l.updated(j, if c == '.' then '#' else '.'))
        val newReflections = updateds.map(updated => findReflections(updated)).flatten
        reflection.map(r => newReflections.filter(_ != r)).getOrElse(newReflections).headOption

    def findReflections(pattern: Vector[Seq[Char]]): Seq[Int] =
        (1 until pattern.length).filter {i =>
            val (left, right) = pattern.splitAt(i)
            left.reverse.take(right.size) == right.take(left.size)
        }


    def parseInput(s: String): Seq[Seq[Seq[Char]]] =
        Source.fromFile(s).getLines().mkString("\n").split("\n\n").toSeq.map(pattern => pattern.split("\n").toSeq.map(_.toCharArray().toSeq))
}