import io.Source

object Part1 {

    def main(args: Array[String]): Unit = 
        val instructions = parseInput(args(0))
        println(instructions.map(hash).sum)

    def hash(s: String) =
        s.foldLeft(0)((v, c) => ((c.toInt + v) * 17) % 256)

    def parseInput(s: String): Seq[String] =
        Source.fromFile(s).getLines().filter(!_.isBlank()).toSeq.head.split(",").toSeq
        
}