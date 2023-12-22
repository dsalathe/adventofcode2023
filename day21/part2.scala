import io.Source
import scala.collection.immutable.Queue

object Part2 {

    val TARGET_STEPS = 26_501_365

    /**
     * By noticing that the input is of form:
     * X . X
     * . S .
     * X . X
     * where X are random shapes and . is a garden plot, we can add three strong assumptions:
     *      - The garden is a square
     *      - The start position is in the exact middle
     *      - we can reach borders with a straight line
     * 
     * We therefore have this configuration:
     * 
     * X . X X . X X . X
     * . . . . . . . . .
     * X . X x . X X . X
     * X . X x . X X . X
     * . . . . S . . . .
     * X . X X . X X . X
     * . . . . . . . . .
     * X . X X . X X . X
     * 
     * We therefore notice that each tile in each direction is reached at the same time for the same depth.
     * if A is the first garden traversed, and Bs are the second etc, we have:
     * 
     * 
     * . . . D . . .
     * . . D C D . .
     * . D C B C D .
     * D C B A B C D
     * . D C B C B .
     * . . D C D . .
     * . . . D . . .
     * 
     * which, when we count it, equals A + 4B + 8C + 12D, which, accumulated, yields: 1, 5, 13, 25 which is a quadratic growth.
     * 
     * Now, notice that 26_501_365 is w * 202300 + 65, where w=131 is the width.
     * It turns out that 65 is the exact number of steps needed to reach every border of the garden. After that, we reach the following ones at 131 + 65,
     * which is the distance to traverse the garden in a straight line or to traverse half of it, turns 90 degrees, and traverse another half of it.
     * It means that after each n*131 + 65, we reach a similar state but with quadratically more gardens at each time.
     * Therefore, sampling results at s0=65, s1=131+65 and s2=2*131+65 will give us points sharing the same quadratic function as in x=202300*131+65.
     * 
     * Now let us stop thinking at the tile level, but rather think at the garden level:
     * let us redefine the "step 0" as the step where we are in a stable state, meaning we are at the 4 borders at the same time, which is 65 "tiles steps"
     * Let define the next step as traversing all neighbor gardens.
     * It lets us redefine s0, s1, s2 as x0=0, x1=1, x2=2 because we shift 65 steps and define a "big" step as 131 little steps.
     * let's run solution1 to get the corresponding y's
     * Then we can extrapolate coefficients with our 3 points (x0, y0), (x1, y1) and (x2, y2) using Lagrange with x's simplifying a lot with x's = [0,1,2]
     * Finally, we extrapolated coefficients, evaluate ysolution = ax² + bx + c with x=202300
      *
      */
    def main(args: Array[String]): Unit =
        val (graph, startPosition) = parseInput(args(0))
        println(startPosition)
        val width = graph.map(_._1.x).max + 1 // Graph is a square
        val shift = 65
        val y0 = bfs(graph, width, startPosition, shift)
        val y1 = bfs(graph, width, startPosition, shift + width)
        val y2 = bfs(graph, width, startPosition, shift + 2*width)
        val (a, b, c) = lagrange(y0, y1, y2)
        val target = (TARGET_STEPS - shift) / width
        println(s"$y0 $y1 $y2")
        println(s"$a $b $c $target")
        println((a * target * target + b * target + c).toLong)

    /**
      * General lagrange formula:
    L₀(x) = (x – x₁) × (x – x₂) / ((x₀ – x₁) × (x₀ – x₂))
    L₁(x) = (x – x₀) × (x – x₂) / ((x₁ – x₀) × (x₁ – x₂))
    L₂(x) = (x – x₀) × (x – x₁) / ((x₂ – x₀) × (x₂ – x₁))

    => y = y₀ × L₀(x) + y₁ × L₁(x) + y₂ × L₂(x)

    using x₀=0, x₁=1 and x₂=2, we have

    L₀(x) = (x-1)(x-2) / (-1) × (-2)) = (x² - 3x + 2) / 2
    L₁(x) = x(x-2) / (1 × (-1)) = -x² + 2x
    L₂(x) = x(x-1) / (2 × 1) = (x² - 2x) / 2

    => y = y₀ (x² -3x + 2) / 2 + y₁ (x² - 2x) + y₂ (x² -2x) / 2
       y = (y₀ / 2 - y₁ + y₂ / 2)x² + (-3y₀/2 -2y₁ - y₂/2)x + y₀

    => a = y₀/2 - y₁ + y₂/2
    => b = -3y₀/2 -2y₁ - y₂/2
    => c = y₀

      */
    def lagrange(y0: Int, y1: Int, y2: Int): (Double, Double, Double) =
        val a = y0 / 2.0 - y1 + y2 / 2.0
        val b = -3.0 * (y0 / 2.0) + 2.0 * y1 - y2 / 2.0
        val c = y0.toDouble
        (a, b, c)  

    def bfs(graph: Map[Position, Char], width: Int, startPosition: Position, maxDepth: Int): Int =
        def helper(q: Queue[(Position, Int)], visited: Set[Position], count: Int): Int =
            if q.isEmpty then
                count
            else
                val ((position, depth), remainingQ) = q.dequeue
                if visited.contains(position) || depth > maxDepth then 
                    helper(remainingQ, visited, count) 
                else
                    val neighbors = position.getNeighbors()
                        .filter(p => graph(Position(positiveModulo(p.x, width), positiveModulo(p.y, width))) != '#')
                        .map(p => (p, depth+1))
                    helper(remainingQ.enqueueAll(neighbors), visited + position, if depth % 2 == maxDepth % 2 then count + 1 else count)
        helper(Queue[(Position, Int)]().enqueue((startPosition, 0)), Set(), 0)

    def positiveModulo(a: Int, b: Int) = ((a % b) + b) % b

    case class Position(x: Int, y: Int):
        def getNeighbors() = Seq(Position(x, y-1), Position(x+1, y), Position(x, y+1), Position(x-1, y))

    def parseInput(s: String): (Map[Position, Char], Position) =
        val lines = Source.fromFile(s).getLines().filter(!_.isBlank())
        val graph = lines.zipWithIndex.flatMap {case (line, y) => line.zipWithIndex.map {case (c, x) => Position(x, y) -> c}}.toMap
        val startPosition = graph.filter{case (position, c) => c == 'S'}.map(_._1).head
        (graph, startPosition)
}