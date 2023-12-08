//> using platform "scala-native"
//> using nativeMode "release-full"
//> using nativeGc "none"

import io.Source

object Part1 {
    
    def main(args: Array[String]): Unit = 
        val (instructions, graph) = parseInput(args(0))
        println(findPathLength("AAA", "ZZZ", graph, instructions))


    def findPathLength(currentNodeId: NodeId, goalNodeId: NodeId, graph: Graph, instructions: Instructions): Int =
        def helper(currentNodeId: NodeId, currentLength: Int): Int =
            if currentNodeId == goalNodeId then currentLength else helper(graph.move(currentNodeId, instructions(currentLength)), currentLength + 1)
        helper(currentNodeId, 0)

    def parseInput(s: String): (Instructions, Graph) =
        val lines = Source.fromFile(s).getLines().filter(!_.isBlank()).toSeq
        val instruction = lines.head.trim() 
        val nodePattern = "([A-Z]+) = [(]([A-Z]+), ([A-Z]+)[)]".r // eg. AAA = (BBB, CCC)
        val nodes =  
            for pm <- nodePattern.findAllMatchIn(lines.tail.mkString("\n"))
            yield Node(pm.group(1), pm.group(2), pm.group(3))

        (Instructions(instruction), Graph(nodes))

    // Alias type for String
    type NodeId = String

    case class Node(id: NodeId, leftId: NodeId, rightId: NodeId):
        def move(instruction: Char) = if instruction == 'L' then leftId else rightId

    // Abstraction of how to interract with nodes
    case class Graph(private val nodes: Iterator[Node]):
        private val mapNodes = nodes.map(n => n.id -> n).toMap

        def move(nodeId: NodeId, instruction: Char): NodeId = mapNodes(nodeId).move(instruction)

    // It's just a simple wrapper of the instruction string, making it safe to interact with using the modulo operator
    case class Instructions(s: String):
        val vector = s.toVector
        def apply(i: Int): Char = vector(i % vector.size)

}