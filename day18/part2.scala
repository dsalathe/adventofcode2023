import io.Source
import scala.collection.mutable.{Stack, HashSet as HSet}

object Part2 {
    // Pick's Theorem: A = i + b/2 -1 with A = area, b = perimeter size, i = number of coordinates inside the polygon
    // => i = A - b/2 + 1
    // total points is therefore: i + b. ==> i + b = A + b/2 + 1
    // A can be computed with the Shoelace formula
    // ====> total points = i + b = A + b/2 + 1 = Shoelace(polygon) + half perimeter + 1

    def main(args: Array[String]): Unit = 
        val instructions = parseInput(args(0))
        val polygon = toPolygon(instructions)
        println(shoelace(polygon) + getPerimeter(polygon) / 2 + 1)

    def toPolygon(instructions: Seq[Instruction]): Seq[Position] =
        instructions.foldLeft(List(Position(0L, 0L)))((polygon, instruction) =>
            val Position(x, y) = polygon.head
            val Instruction(direction, length) = instruction
            val nextPosition = direction match
                case Direction.R => Position(x + length, y)
                case Direction.D => Position(x, y + length)
                case Direction.L => Position(x - length, y)
                case Direction.U => Position(x, y - length)
            nextPosition +: polygon           
        )


    def getPerimeter(polygon: Seq[Position]): Long =
        (polygon zip polygon.tail).map {case (p1, p2) => math.abs(p1.x - p2.x) + math.abs(p1.y - p2.y)}.sum

    def shoelace(polygon: Seq[Position]): Long = 
        math.abs((polygon zip polygon.tail).map {case (p1, p2) => p1.x*p2.y - p1.y*p2.x}.sum / 2)


    enum Direction:
        case R, D, L, U

    case class Instruction(direction: Direction, length: Int)
    case class Position(x: Long, y: Long)

    def parseInput(s: String): Seq[Instruction] =
        val lines = Source.fromFile(s).getLines().filter(!_.isBlank())
        lines.map(line => line.split(" ")).map{case Array(_, _, color) =>
            val colorEncoded = color.tail.dropRight(1)
            Instruction(Direction.fromOrdinal(colorEncoded.takeRight(1).toInt), Integer.parseInt(colorEncoded.tail.dropRight(1), 16))    
        }.toSeq
}