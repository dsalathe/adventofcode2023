import io.Source

object Part1 {

    def main(args: Array[String]): Unit =
        val bricks = parseInput(args(0))
        val fallenBricks = fallBricks(bricks)
        val superposingMap = buildSuperpositionMap(fallenBricks)
        val superposedByMap = buildSuperposedByMap(fallenBricks)
        println(fallenBricks.filter(b => canBeRemoved(superposedByMap = superposedByMap, superposingMap = superposingMap, brick = b )).size)

    case class Position(x: Int, y: Int, z: Int):
        def fall(distance: Int): Position = Position(x, y, z - distance)
        
    case class Brick(p1: Position, p2: Position):
        def fall(distance: Int): Brick = Brick(p1.fall(distance), p2.fall(distance))
        def bottom: Int = math.min(p1.z, p2.z)
        def up: Int = math.max(p1.z, p2.z)
        def left: Int = math.min(p1.x, p2.x)
        def front: Int = math.min(p1.y, p2.y)
        def isSuperposedWith(other: Brick): Boolean =
            val (minX, maxX) = if this.left < other.left then (this, other) else (other, this)
            val doesXOverlap = math.max(minX.p1.x, minX.p2.x) >= math.min(maxX.p1.x, maxX.p2.x)
            lazy val (minY, maxY) = if this.front < other.front then (this, other) else (other, this)
            lazy val doesYOverlap = math.max(minY.p1.y, minY.p2.y) >= math.min(maxY.p1.y, maxY.p2.y)
            doesXOverlap && doesYOverlap

    def canBeRemoved(superposedByMap: Map[Brick, Seq[Brick]], superposingMap: Map[Brick, Seq[Brick]], brick: Brick) : Boolean =
        superposedByMap(brick).forall(above => superposingMap(above).size > 1)
    
    def buildSuperposedByMap(fallenBricks: Seq[Brick]): Map[Brick, Seq[Brick]] =
        fallenBricks.map { brick =>
            brick -> fallenBricks.filter(other => brick.isSuperposedWith(other) && other.bottom == brick.up + 1)     
        }.toMap
    
    def buildSuperpositionMap(fallenBricks: Seq[Brick]): Map[Brick, Seq[Brick]] =
        fallenBricks.map { brick =>
            brick -> fallenBricks.filter(other => brick.isSuperposedWith(other) && brick.bottom == other.up + 1)     
        }.toMap
    
    def fallBricks(bricks: Seq[Brick]): Seq[Brick] = 
        val sortedBricks = bricks.sortBy(_.bottom)
        val floor: Brick = Brick(Position(Int.MinValue, Int.MinValue, 0), Position(Int.MaxValue, Int.MaxValue, 0))
        sortedBricks.foldLeft(Seq(floor)) { case (fallens, nextFalling) =>
            val supportingBrick = fallens.filter(nextFalling.isSuperposedWith).sortBy(-_.up).head // floor ensures there is one
            val fallingDistance = nextFalling.bottom - supportingBrick.up - 1
            nextFalling.fall(fallingDistance) +: fallens
        }

    def parseInput(s: String): Seq[Brick] =
        val lines = Source.fromFile(s).getLines().filter(!_.isBlank())
        lines.map { line =>
            line.split("~") match
                case Array(left, right) =>
                        def lineToPosition(l: String): Position = l.split(",").map(_.toInt) match
                            case Array(x, y, z) => Position(x, y, z)
                            case _ => throw AssertionError()
                        Brick(lineToPosition(left), lineToPosition(right))
                case _ => throw AssertionError()
        }.toSeq
}