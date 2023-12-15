import io.Source
import scala.collection.mutable.ArrayBuffer

object Part1 {

    def main(args: Array[String]): Unit = 
        val platform = parseInput(args(0)).reverse.transpose
        tiltEast(platform)
        val tiltedPlatform = platform.transpose.reverse
        println(countWeights(tiltedPlatform))


    def countWeights(platform: ArrayBuffer[ArrayBuffer[Char]]): Int =
        val size = platform.size
        platform.zipWithIndex.map{case (line, i) => line.filter(_ == 'O').size * (size - i)}.sum
    
    def tiltEast(platform: ArrayBuffer[ArrayBuffer[Char]]): Unit =
        (1 to platform.head.size).map {_ =>
            (0 until platform.size).map{j =>
                (0 until (platform(j).size - 1)).foreach { i =>
                    if platform(j)(i) == 'O' && platform(j)(i+1) == '.' then
                        platform(j)(i) = '.'
                        platform(j)(i+1) = 'O'
                }
            }
        }


    def parseInput(s: String): ArrayBuffer[ArrayBuffer[Char]] =
        val lines = Source.fromFile(s).getLines().filter(!_.isBlank())
        lines.map(_.to(ArrayBuffer)).to(ArrayBuffer)
}