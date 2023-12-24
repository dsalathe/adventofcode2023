//> using python
 
//TODO WIP, seems to break on my machine

import io.Source
import me.shadaj.scalapy.py

object Part2 {

    def main(args: Array[String]): Unit = 
        val hailstones = parseInput(args(0))
        val z3 = py.module("z3")
        println(
            z3
        )

    
    def computeRockPosition(hailstones: Seq[HailStone]): Long = 
        ???



    case class HailStone(x: Long, y: Long, z: Long, dx: Long, dy: Long, dz: Long)

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